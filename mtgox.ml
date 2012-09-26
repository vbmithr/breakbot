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
            "params", `String (Opt.unopt ~default:"" params)]

  let json_id_of_query = function
    | `Assoc l -> List.assoc "id" l
    | _ -> failwith "id_of_query"

end

open Protocol

class mtgox key secret =
object (self)
  inherit Exchange.exchange "mtgox"

  val mutable ic = Lwt_io.zero
  val mutable oc = Lwt_io.null

  val key     = key
  val cmd_buf = Bi_outbuf.create 4096
  (* To replace with unimplemented hmac_sha512 required for MtGox *)
  val hmac    = CK.MAC.hmac_sha256 secret

  method private set_ic newic = ic <- newic
  method private set_oc newoc = oc <- newoc

  method update =
    let buf = Bi_outbuf.create 4096 in
    let rec update (ic, oc) =
      let rec main_loop () =
        lwt line = Lwt_io.read_line ic in
        lwt () = Lwt_io.printf "%s\n" line in
        lwt () = self#notify in
        main_loop () in

      (* Initialisation: Unsubscribe from some channels *)
      self#set_ic ic;
      self#set_oc oc;
      lwt () = Lwt_io.write_line oc (Yojson.Safe.to_string ~buf
                                       (unsubscribe Ticker)) in
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
    Lwt_io.write_line oc res

  method bid curr price amount = Lwt.return ()
  method ask curr price amount = Lwt.return ()
end
