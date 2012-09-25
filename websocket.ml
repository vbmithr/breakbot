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

exception Response_failure of response_error

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

let with_websocket uri f =
  lwt myhostname = Lwt_unix.gethostname () in
  let uri = Uri.of_string uri in
  (* let host = Opt.unopt (Uri.host uri) in *)
  (* let port = string_of_int (Opt.unopt ~default:80 (Uri.port uri)) in *)
  let random_bits = Random.bits () in
  let nonce = String.create 2 in
  let _ =
    nonce.[0] <- Char.chr ((random_bits lsr 8) land (1 lsl 8 - 1));
    nonce.[1] <- Char.chr (random_bits land (1 lsl 8 - 1)) in
  let nonce64 = Cohttp.Base64.encode nonce in
  let headers =
    Cohttp.Header.of_list
      ["Host"                  , myhostname;
       "Upgrade"               , "websocket";
       "Connection"            , "Upgrade";
       "Sec-WebSocket-Key"     , nonce64;
       "Sec-WebSocket-Version" , "13"] in
  lwt ic, oc = Net.connect_uri uri in
  let req = Request.make ~headers uri in
  lwt () = Client.write_request req oc in
  lwt res = Client.read_response ic oc in
  let response, _ =
    try Opt.unopt res
    with Opt.Unopt_none -> raise (Response_failure Response_empty) in
  let () =
    check_response_is_conform response nonce64 in

  let fun_ic, my_oc = Lwt_io.pipe ()
  and my_ic, fun_oc = Lwt_io.pipe () in

  (* buffer to store the frame *)
  let buf_frame = String.make (Lwt_io.default_buffer_size ()) '\000' in

  let rec read_frames () =
    lwt hdr = Lwt_io.read ~count:2 ic in
    let final = hdr.[0] > '\127' in
    let () = Printf.printf "This is a final frame: %b.\n%!" final in
    let opcode = Opcode.of_int (Char.code hdr.[0]) in
    let () = Printf.printf "This frame is a %s.\n%!" (Opcode.to_string opcode) in
    let masked = hdr.[1] > '\127' in
    lwt payload_len = match (Char.code hdr.[1]) land 127 with
      | i when i < 126 -> Lwt.return i
      | 126 -> Lwt_io.BE.read_int16 ic
      | 127 -> (Lwt_io.BE.read_int64 ic) >>= fun i -> Lwt.return (Int64.to_int i)
      | _ -> failwith "Can never happen." in
    let () = Printf.printf "Masked: %b. Payload len: %d\n%!" masked payload_len in
    lwt mask_key = if masked then Lwt_io.read ~count:4 ic else Lwt.return "" in
    let rec read_payload = function
      | 0 -> Lwt.return ()
      | n -> (lwt bytes_read = Lwt_io.read_into ic buf_frame 0
                (min n (Lwt_io.default_buffer_size ())) in
              lwt () = Lwt_io.write_from_exactly my_oc buf_frame 0 bytes_read in
              read_payload (n - bytes_read)) in
    lwt () = read_payload payload_len in
    lwt () = Lwt_io.write_char my_oc '\n' in
    read_frames ()

  in
  Lwt.join [read_frames (); f (fun_ic, fun_oc)]
