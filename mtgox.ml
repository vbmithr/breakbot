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

  let query ?(optfields=[]) endpoint =
    let nonce = Printf.sprintf "%.0f" (Unix.gettimeofday () *. 1e6) in
    let id = Digest.to_hex (Digest.string nonce) in
    let assoc =
      ["id", `String id; "nonce", `String nonce; "call", `String endpoint] in
    match optfields with
      | [] -> `Assoc assoc
      | l  -> `Assoc (assoc @ l)

  let get_depth = query
    ~optfields:["item", `String "BTC"; "currency", `String "USD"] "depth"

  let get_ticker = query
    ~optfields:["item", `String "BTC"; "currency", `String "USD"] "ticker"

  let get_currency_info = query
    ~optfields:["params", `Assoc ["currency", `String "USD"]] "currency"

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

  method private set_buf_in b  = buf_in <- b
  method private set_buf_out b = buf_out <- b

  method update =
    let buf_json_in = Buffer.create 4096 in
    let rec update (buf_in, buf_out) =
      self#set_buf_in buf_in;
      self#set_buf_out buf_out;

      let rec main_loop () =
        lwt (_:int) = Sharedbuf.with_read buf_in (fun buf len ->
          lwt () =
            (if len > 0 then (* Uncomplete message *)
                Lwt.return (Buffer.add_string buf_json_in (String.sub buf 0 len))
             else
                let () = Printf.printf "%s\n%!" (Buffer.contents buf_json_in) in
                let () = Buffer.clear buf_json_in in (* maybe use reset here ?*)
                self#notify)
          in Lwt.return len)
        in
        main_loop () in

      lwt () = Sharedbuf.write_lines buf_out
        [(Yojson.Safe.to_string (unsubscribe Ticker));
         (Yojson.Safe.to_string (unsubscribe Trade))] in
      (* lwt (_:int) = self#command (Protocol.query "private/info") in *)
      lwt (_:int) = self#command (Protocol.get_depth) in
      (* lwt (_:int) = self#command (Protocol.get_currency_info) in *)

      main_loop () in
    Websocket.with_websocket "http://websocket.mtgox.com/mtgox" update

  method private command (query:json) =
    let query_json = Yojson.Safe.to_string ~buf:cmd_buf query in
    let signed_query = CK.hash_string (CK.MAC.hmac_sha512 secret) query_json in
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
