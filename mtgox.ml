open Utils
open Yojson.Safe

module CK = Cryptokit

module Protocol = struct

  type channel = Trade | Ticker | Depth

  let channel_of_string = function
    | "24e67e0d-1cad-4cc0-9e7a-f8523ef460fe" -> Depth
    | "d5f06780-30a8-4a48-a2f8-7ed181b4a13f" -> Ticker
    | "dbf1dee9-4f2e-4a08-8cb7-748919a71b21" -> Trade
    | _ -> failwith "channel_of_string"

  let string_of_channel = function
    | Depth  -> "24e67e0d-1cad-4cc0-9e7a-f8523ef460fe"
    | Ticker -> "d5f06780-30a8-4a48-a2f8-7ed181b4a13f"
    | Trade  -> "dbf1dee9-4f2e-4a08-8cb7-748919a71b21"

  type op_type =
    | Subscribe
    | Unsubscribe
    | Remark
    | Private
    | Result
    | Call
    | MtgoxSubscribe

  let subscribe channel =
    `Assoc ["op", `String "subscribe"; "channel",
            `String (string_of_channel channel)]

  let unsubscribe channel =
    `Assoc ["op", `String "unsubscribe"; "channel",
            `String (string_of_channel channel)]

  let query ?params endpoint =
    let nonce = Printf.sprintf "%.0f" (Unix.gettimeofday () *. 1e6) in
    let id = Digest.to_hex (Digest.string nonce) in
    `Assoc ["id", `String id; "nonce", `String nonce; "call", `String endpoint;
            "params", `String (Opt.unopt ~default:"" params);
            "item", `String "BTC"]

  let get_depth = query "BTCUSD/depth"

  let json_id_of_query = function
    | `Assoc l -> List.assoc "id" l
    | _ -> failwith "id_of_query"

end

open Protocol

class mtgox key secret =
object (self)
  inherit Exchange.exchange "mtgox"

  val mutable buf_in = Sharedbuf.empty ~bufsize:0 ()
  val mutable buf_out = Sharedbuf.empty ~bufsize:0 ()

  val key     = key
  val cmd_buf = Bi_outbuf.create 4096
  val hmac    = CK.MAC.hmac_sha512 secret

  method private set_buf_in b  = buf_in <- b
  method private set_buf_out b = buf_out <- b

  method update =
    let buf = Bi_outbuf.create 4096 in
    let rec update (buf_in, buf_out) =
      self#set_buf_in buf_in;
      self#set_buf_out buf_out;

      let rec main_loop () =
        lwt (_:int) = Sharedbuf.with_read buf_in (fun buf len ->
          Printf.printf "%s\n" (String.sub buf 0 len);
          lwt () = self#notify in Lwt.return len)
        in
        main_loop () in

      lwt (_:int) = Sharedbuf.write_line buf_out
        (Yojson.Safe.to_string ~buf (unsubscribe Ticker)) in
      lwt (_:int) = Sharedbuf.write_line buf_out
        (Yojson.Safe.to_string ~buf (unsubscribe Trade)) in
      (* lwt () = self#command Protocol.get_depth in *)

      main_loop () in
    Websocket.with_websocket "http://websocket.mtgox.com/mtgox" update

  method private command (query:json) =
    let query_json = Yojson.Safe.to_string ~buf:cmd_buf query in
    let signed_query =
      hmac#wipe;
      hmac#add_string query_json;
      hmac#result in
    let signed_request64 = Cohttp.Base64.encode
      (key ^ signed_query ^ query_json) in
    let res = Yojson.Safe.to_string ~buf:cmd_buf
      (`Assoc ["op", `String "call";
               "context", `String "mtgox.com";
               "id", json_id_of_query query;
               "call", `String signed_request64]) in
    Sharedbuf.write_line buf_out res

  method bid curr price amount = Lwt.return ()
  method ask curr price amount = Lwt.return ()
end
