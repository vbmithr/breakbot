open Utils
open Common
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
    let nonce = Unix.getmicrotime_str () in
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

  module LL = struct
    type depth =
        {
          currency: string;
          item: string;
          now: string;
          price: string;
          price_int: string;
          total_volume_int: string;
          type_: int;
          type_str: string;
          volume: string;
          volume_int: string;
        } with rpc ("type_" -> "type")

    type depth_msg =
        {
          channel: string;
          depth: depth;
          op: string;
          origin: string;
          private_: string;
        } with rpc ("private_" -> "private")
  end

  module HL = struct
    type depth =
        {
          curr: Currency.t;
          kind: Order.kind;
          price: int64;
          amount: int64;
          ts: int64
        }

    let of_depth (d:LL.depth_msg) =
      let d = d.LL.depth in
      {
        curr=Currency.of_string d.LL.currency;
        kind=Order.kind_of_string d.LL.type_str;
        price=Int64.of_string d.LL.price_int;
        amount=Int64.of_string d.LL.total_volume_int;
        ts=Int64.of_string d.LL.now
      }
  end

end

module Parser = struct
  open Protocol
  let parse_depth books rpc =
    let ll = LL.depth_msg_of_rpc rpc in
    let hl = HL.of_depth ll in
    Books.add ~ts:hl.HL.ts books
      hl.HL.curr hl.HL.kind hl.HL.price hl.HL.amount

  let parse_orderbook books decoder =
    let rec parse_orderbook ctx books =
      let unstr = function `String str -> str | _ -> failwith "unstr" in

      let parse_array ctx books =
        let rec parse_array acc books =
          match Jsonm.decode decoder with
            | `Lexeme (`Name s) -> (match Jsonm.decode decoder with
                | `Lexeme lex ->
                  parse_array ((s, lex)::acc) books
                | _ -> failwith "parse_array")
            | `Lexeme `Oe ->
              let ts = Int64.of_string (List.assoc "stamp" acc |> unstr)
              and price = Int64.of_string
                (List.assoc "price_int" acc |> unstr)
              and amount = Int64.of_string
                (List.assoc "amount_int" acc |> unstr) in
              let books =
                Books.add ~ts books (Currency.USD) (Order.kind_of_string ctx)
                  price amount in
              parse_array [] books
            | `Lexeme `Ae -> parse_orderbook "" books
            | _ -> parse_array acc books in
        parse_array [] books in

      match Jsonm.decode decoder with
        | `Lexeme `Os       -> parse_orderbook ctx books
        | `Lexeme (`Name s) -> parse_orderbook s books
        | `Lexeme `As       -> parse_array ctx books
        | `Lexeme _         -> parse_orderbook "" books
        | `Error e          -> Jsonm.pp_error Format.err_formatter e; books
        | `Await            -> books
        | `End              -> books
    in parse_orderbook "" books

  let rec parse books json_str =
    if (String.length json_str) > 4096 then (* It is the depths *)
      let decoder = Jsonm.decoder (`String json_str) in
      Lwt.return (parse_orderbook books decoder)
    else
      try_lwt
        let rpc = Jsonrpc.of_string json_str in
        Lwt.choose [Lwt.wrap2 parse_depth books rpc]
      with e ->
        Printf.printf "Failed to parse automatically: %s\n"
          (Printexc.to_string e);
        (* Automatic parsing failed *)
        let decoder = Jsonm.decoder (`String json_str) in
        Lwt.return (parse_orderbook books decoder)
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
                let buf_str = Buffer.contents buf_json_in in
                let () = Printf.printf "%s\n%!" buf_str in
                lwt new_books = Parser.parse books buf_str in
                let () = books <- new_books in
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
