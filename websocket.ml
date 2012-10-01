open Utils
open Common
open Mycohttp

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

module Opcode = struct
  type t =
    | Frame_continuation
    | Frame_text
    | Frame_binary
    | Frame_close
    | Frame_ping
    | Frame_pong
    | Frame_oth_ctrl
    | Frame_oth_nonctrl

  let to_string = function
    | Frame_continuation -> "continuation frame"
    | Frame_text         -> "text frame"
    | Frame_binary       -> "binary frame"
    | Frame_close        -> "close frame"
    | Frame_ping         -> "ping frame"
    | Frame_pong         -> "pong frame"
    | Frame_oth_ctrl     -> "other control frame"
    | Frame_oth_nonctrl  -> "other non-control frame"

  let of_int i = match i land 0xf with
    | 0 -> Frame_continuation
    | 1 -> Frame_text
    | 2 -> Frame_binary
    | 8 -> Frame_close
    | 9 -> Frame_ping
    | 10 -> Frame_pong
    | i when i > 2 && i < 8 -> Frame_oth_nonctrl
    | _ -> Frame_oth_ctrl

  let to_int op = function
    | Frame_continuation -> 0
    | Frame_text         -> 1
    | Frame_binary       -> 2
    | Frame_close        -> 8
    | Frame_ping         -> 9
    | Frame_pong         -> 10
    | _ -> failwith "Opcode.to_char: Invalid frame type"
end

let check_response_is_conform resp nonce64 =
  let check_sec_websocket_accept accept nonce64 =
    let hash = Cohttp.Base64.encode
      (CK.hash_string (CK.Hash.sha1 ()) (nonce64 ^ websocket_uuid)) in
    if accept <> hash
    then raise (Response_failure Bad_sec_websocket_accept) in
  let hdrs = Response.headers resp in
  let hdrs = Cohttp.Header.to_list hdrs in
  let () = List.iter (fun (a, b) -> Printf.printf "%s: %s\n%!" a b) hdrs in

  (try (match List.assoc "upgrade" hdrs with
    | "WebSocket" | "websocket" -> ()
    | str         -> raise (Response_failure (Bad_upgrade_hdr str)))
   with Not_found -> raise (Response_failure (Bad_upgrade_hdr "")));

  (try (match List.assoc "connection" hdrs with    | "Upgrade" | "upgrade" -> ()
    | str       -> raise (Response_failure (Bad_connection_hdr str)))
   with Not_found -> raise (Response_failure (Bad_connection_hdr "")));

  (try
    let str = List.assoc "sec-websocket-accept" hdrs in
    check_sec_websocket_accept str nonce64
   with Not_found -> raise (Response_failure Bad_sec_websocket_accept));

  if Response.version resp <> `HTTP_1_1
  then raise (Response_failure Bad_http_version);
  if Response.status resp <> `Switching_protocols
  then raise (Response_failure Bad_status_code)

let with_websocket uri_string f =
  (* Initialisation *)
  lwt myhostname = Lwt_unix.gethostname () in
  let uri = Uri.of_string uri_string in

  let buf_in = Sharedbuf.empty ()
  and buf_out = Sharedbuf.empty () in

  let connect () =
    let nonce = Random.int16_as_string () in
    let nonce64 = Cohttp.Base64.encode nonce in
    let headers =
      Cohttp.Header.of_list
        ["Host"                  , myhostname;
         "Upgrade"               , "websocket";
         "Connection"            , "Upgrade";
         "Sec-WebSocket-Key"     , nonce64;
         "Sec-WebSocket-Version" , "13"] in
    let req = Request.make ~headers uri in
    lwt ic, oc = Net.connect_uri uri in
    lwt () = Client.write_request req oc in
    lwt res = Client.read_response ic oc in
    let response, _ =
      try Opt.unopt res
      with Opt.Unopt_none -> raise (Response_failure Response_empty) in
    let () = check_response_is_conform response nonce64 in
    let () = Printf.printf "(Re)connected to %s\n%!" uri_string in
    Lwt.return (ic, oc)
  in

  let rec read_frames ic =
    let hdr = String.create 2 in
    lwt () = Lwt_io.read_into_exactly ic hdr 0 2 in
    let final = hdr.[0] > '\127' in
    let opcode = Opcode.of_int (Char.code hdr.[0]) in
    let () = Printf.printf "Read a final: %b frame of type %s\n%!"
      final (Opcode.to_string opcode) in
    match opcode with
      | Opcode.Frame_text ->
        (let () = if hdr.[1] > '\127' then
            raise (Response_failure Frame_masked) in
         lwt payload_len = match (Char.code hdr.[1]) land 127 with
           | i when i < 126 -> Lwt.return i
           | 126 -> Lwt_io.BE.read_int16 ic
           | 127 -> lwt len = (Lwt_io.BE.read_int64 ic)
                    in Lwt.return (Int64.to_int len)
           | _ -> failwith "Can never happen." in
         let rec read_payload = function
           | 0 -> Lwt.return ()
           | n ->
             lwt written = Sharedbuf.with_write buf_in
               (fun buf ->
                 Lwt_io.read_into ic buf 0 (String.length buf))
             in
             let () = Printf.printf "MtGox -> Me written in the shbuf\n%!" in
             read_payload (n - written) in

         lwt () = read_payload payload_len in
         read_frames ic)

      | _ -> raise Operation_not_supported
  in

  let rec write_frames oc =
    let mask = Random.int32_as_string () in

    let read_fun buf len =
      lwt () = if len = String.length buf then
          Lwt_io.write_char oc '\001' else (* Message need more than one frame *)
          Lwt_io.write_char oc '\129' in (* Message can be sent in one frame *)
      lwt () =
        (match len with
          | n when n < 126 ->
            Lwt_io.write_char oc (Char.chr (128+n))
          | n when n < (1 lsl 16) ->
            lwt () = Lwt_io.write_char oc '\254' in
            Lwt_io.BE.write_int16 oc n
          | n ->
            lwt () = Lwt_io.write_char oc '\255' in
            Lwt_io.BE.write_int64 oc (Int64.of_int n)) in
      lwt () = Lwt_io.write_from_exactly oc mask 0 4 in
      let () = for i = 0 to len - 1 do (* masking msg to send *)
          buf.[i] <- Char.chr
            ((Char.code mask.[i mod 4]) lxor (Char.code buf.[i]))
        done in
      lwt () = Lwt_io.write_from_exactly oc buf 0 len in
      lwt () = Lwt_io.flush oc in
      Lwt.return len in

    (* Body of the function *)
    lwt nb_read = Sharedbuf.with_read buf_out read_fun in
    let () = Printf.printf "Written %d bytes into the websocket\n%!" nb_read in
    write_frames oc

  in
  let rec run_everything () =
    try_lwt
      lwt () = Lwt_unix.sleep 1.0 in (* Do not try to reconnect too fast *)
      lwt ic,oc = connect () in
      lwt () = Lwt.pick [read_frames ic;
                         write_frames oc;
                         f (buf_in, buf_out)] in
      run_everything ()
    with
      | exn ->
        Printf.printf "Lost websocket connection: %s\n%s\n%!"
          (Printexc.to_string exn) (Printexc.get_backtrace ());
        run_everything ()
  in run_everything ()
