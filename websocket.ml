open Utils
open Lwt_utils
open Cohttp
open Cohttp_lwt_unix

module CK = Cryptokit

type response_error =
  | Response_empty
  | Bad_http_version
  | Bad_status_code
  | Bad_upgrade_hdr of string
  | Bad_connection_hdr of string
  | Bad_sec_websocket_accept
  | Frame_masked

exception Response_failure of response_error
exception Operation_not_supported

let websocket_uuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

type opcode =
  [ `Continuation
  | `Text
  | `Binary
  | `Close
  | `Ping
  | `Pong
  | `Ctrl
  | `Nonctrl
  ]

let string_of_opcode = function
  | `Continuation -> "continuation frame"
  | `Text         -> "text frame"
  | `Binary       -> "binary frame"
  | `Close        -> "close frame"
  | `Ping         -> "ping frame"
  | `Pong         -> "pong frame"
  | `Ctrl         -> "other control frame"
  | `Nonctrl      -> "other non-control frame"

let opcode_of_int i = match i land 0xf with
  | 0                     -> `Continuation
  | 1                     -> `Text
  | 2                     -> `Binary
  | 8                     -> `Close
  | 9                     -> `Ping
  | 10                    -> `Pong
  | i when i > 2 && i < 8 -> `Nonctrl
  | _                     -> `Ctrl

let int_of_opcode = function
  | `Continuation -> 0
  | `Text         -> 1
  | `Binary       -> 2
  | `Close        -> 8
  | `Ping         -> 9
  | `Pong         -> 10
  | _             -> failwith "int_of_opcode: Invalid frame type"

let check_response_is_conform resp nonce64 =
  let check_sec_websocket_accept accept nonce64 =
    let hash = Base64.encode
      (CK.hash_string (CK.Hash.sha1 ()) (nonce64 ^ websocket_uuid)) in
    if accept <> hash
    then raise (Response_failure Bad_sec_websocket_accept) in
  let hdrs = Response.headers resp in
  let hdrs = Header.to_list hdrs in

  (try (match List.assoc "upgrade" hdrs with
    | "WebSocket" | "websocket" -> ()
    | str         -> raise (Response_failure (Bad_upgrade_hdr str)))
   with Not_found -> raise (Response_failure (Bad_upgrade_hdr ""))
  );

  (try (match List.assoc "connection" hdrs with
    | "Upgrade" | "upgrade" -> ()
    | str       -> raise (Response_failure (Bad_connection_hdr str)))
   with Not_found -> raise (Response_failure (Bad_connection_hdr ""))
  );

  (try
    let str = List.assoc "sec-websocket-accept" hdrs in
    check_sec_websocket_accept str nonce64
   with Not_found -> raise (Response_failure Bad_sec_websocket_accept)
  );

  if Response.version resp <> `HTTP_1_1
  then raise (Response_failure Bad_http_version);
  if Response.status resp <> `Switching_protocols
  then raise (Response_failure Bad_status_code)

let xor mask msg =
  for i = 0 to String.length msg - 1 do (* masking msg to send *)
    msg.[i] <- Char.chr $
      Char.code mask.[i mod 4] lxor Char.code msg.[i]
  done

let rec read_frames ic push =
  let hdr = String.create 2 in
  lwt () = Lwt_io.read_into_exactly ic hdr 0 2 in
  let hdr = Bitstring.bitstring_of_string hdr in
  let fin, rsv1, rsv2, rsv3, opcode, masked, length =
    bitmatch hdr with
      | { fin: 1; rsv1: 1; rsv2: 1; rsv3: 1;
          opcode: 4; masked: 1; length: 7 }
        -> fin, rsv1, rsv2, rsv3, opcode, masked, length in
  let opcode = opcode_of_int opcode in
  lwt payload_len = match length with
    | i when i < 126 -> Lwt.return i
    | 126            -> Lwt_io.BE.read_int16 ic
    | 127            -> Lwt_io.BE.read_int64 ic >|= Int64.to_int
    | _              -> failwith "Can never happen."
  in
  let mask = String.create 2 in
  lwt () = if masked then Lwt_io.read_into_exactly ic mask 0 2
    else Lwt.return () in
  let buf = String.create payload_len in
  match opcode with
    | `Text ->
      Lwt_io.read_into_exactly ic buf 0 payload_len
      >|= (fun () -> if masked then xor mask buf; push (Some buf))
      >|= (fun () -> if fin then push (Some "")) >> read_frames ic push

    | _ ->
      Lwt_io.read_into_exactly ic buf 0 payload_len
      >> Lwt_log.notice_f
        "Operation not supported: Opcode %d, message:\n%s\n%!"
        (int_of_opcode opcode) buf
      >> raise_lwt Operation_not_supported

let write_frames ~masked stream oc =
  let send_msg ~final ~opcode str =
    let mask = CK.Random.string CK.Random.secure_rng 4 in
    let len = String.length str in
    let first_nibble = 8
    and opcode = int_of_opcode `Text
    and masked = true
    and payload_len = match len with
      | n when n < 126      -> len
      | n when n < 1 lsl 16 -> 126
      | _                   -> 127 in
    let bitstring = Bitstring.string_of_bitstring $
      BITSTRING {first_nibble: 4; opcode: 4; masked : 1; payload_len: 7} in
    lwt () = Lwt_io.write oc bitstring in
    lwt () =
      (match len with
        | n when n < 126        -> Lwt.return ()
        | n when n < (1 lsl 16) -> Lwt_io.BE.write_int16 oc n
        | n                     -> Lwt_io.BE.write_int64 oc $ Int64.of_int n)
    in
    lwt () = if masked then Lwt_io.write_from_exactly oc mask 0 4
        >|= fun () -> xor mask str else Lwt.return () in
    lwt () = Lwt_io.write_from_exactly oc str 0 len in
    Lwt_io.flush oc in

  (* Body of the function *)
  let rec main_loop prev opcode =
    lwt next = Lwt_stream.next stream in
    match (prev, next) with
      | "", ""     -> main_loop "" `Text
      | prev, ""   -> send_msg ~final:true ~opcode prev
        >> main_loop "" `Text
      | "", next   -> main_loop next `Text
      | prev, next -> send_msg ~final:false ~opcode prev
        >> main_loop next `Continuation
  in
  main_loop "" `Text

let with_websocket uri_string f =
  (* Initialisation *)
  lwt myhostname = Lwt_unix.gethostname () in
  let uri        = Uri.of_string uri_string in
  let host       = Opt.unbox (Uri.host uri) in
  let port       = Opt.default 80 (Uri.port uri) in

  let stream_in, push_in   = Lwt_stream.create ()
  and stream_out, push_out = Lwt_stream.create () in

  let connect () =
    let nonce = CK.Random.string CK.Random.secure_rng 2 in
    let nonce64 = Base64.encode nonce in
    let headers =
      Header.of_list
        ["Upgrade"               , "websocket";
         "Connection"            , "Upgrade";
         "Sec-WebSocket-Key"     , nonce64;
         "Sec-WebSocket-Version" , "13"] in
    let req = Request.make ~headers uri in
    let sock_fun fd = Lwt_unix.setsockopt fd Lwt_unix.TCP_NODELAY true in
    lwt sockaddr = Lwt_io.sockaddr_of_dns host (string_of_int port) in
    lwt ic, oc =
      Lwt_io.open_connection ~sock_fun sockaddr in
    lwt () = Request.write (fun _ _ -> Lwt.return ()) req oc in
    lwt response = Lwt.bind_opt $ Response.read ic in
    lwt () = Lwt.wrap2 check_response_is_conform response nonce64 in
    lwt () = Lwt_log.notice_f "(Re)connected to %s\n%!" uri_string in
    Lwt.return (ic, oc)
  in
  lwt ic, oc = connect () in
  Lwt.join [read_frames ic push_in;
            write_frames ~masked:true stream_out oc;
            f (stream_in, push_out)]

let establish_server ?buffer_size ?backlog sockaddr f =
  let stream_in, push_in   = Lwt_stream.create ()
  and stream_out, push_out = Lwt_stream.create () in

  let server_fun (ic,oc) =
    lwt request = Lwt.bind_opt $ Request.read ic in
    let meth    = Request.meth request
    and version = Request.version request
    and uri     = Request.uri request
    and headers = Request.headers request in
    let key = Opt.unbox $ Header.get headers "sec-websocket-key" in
    let () = begin
      assert (meth = `GET);
      assert (version = `HTTP_1_1);
      assert (Opt.map (fun str -> String.lowercase str)
                $ Header.get headers "upgrade" = Some "websocket");
      assert (Opt.map (fun str -> String.lowercase str)
                $ Header.get headers "connection" = Some "upgrade")
    end in
    let hash = Base64.encode
      (CK.hash_string (CK.Hash.sha1 ()) (key ^ websocket_uuid)) in
    let response_headers = Header.of_list
      ["Upgrade", "websocket";
       "Connection", "Upgrade";
       "Sec-WebSocket-Accept", hash] in
    let response = Response.make ~status:`Switching_protocols
      ~headers:response_headers () in
    lwt () = Response.write (fun _ _ -> Lwt.return ()) response oc
    in
    Lwt.join [read_frames ic push_in;
              write_frames ~masked:false stream_out oc;
              f uri (stream_in, push_out)]
  in
  Lwt_io.establish_server ?buffer_size ?backlog sockaddr
    (fun (ic,oc) -> Lwt.ignore_result $ server_fun (ic,oc))
