(*
 * Copyright (c) 2012 Vincent Bernardoff <vb@luminar.eu.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Utils
open Lwt_utils
open Jsonrpc_utils
open Common

module CoUnix = Cohttp_lwt_unix

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
    ["op", "subscribe"; "channel", string_of_channel channel]

  let unsubscribe channel =
    ["op", "unsubscribe"; "channel", string_of_channel channel]

  type async_message = (string * string) list with rpc
  type params        = (string * string) list with rpc
  type query  =
      {
        fields: params;
        params: params
      }

  let rpc_of_query q =
    let res = rpc_of_params q.fields in match (q.params, res) with
      | [], res -> res
      | l, Rpc.Dict v -> Rpc.Dict (("params", rpc_of_params l) :: v)
      | _ -> failwith "rpc_of_query"

  let query_of_rpc = function
    | Rpc.Dict fields ->
      let fields, params = List.partition
        (fun(k,v) -> k <> "params") fields in
      {
        fields=params_of_rpc $ Rpc.Dict fields;
        params=params_of_rpc $ Rpc.Dict params
      }
    | _ -> failwith "query_of_rpc"

  type currency_info =
      {
        currency      : string;
        display       : string;
        display_short : string;
        value         : string;
        value_int     : string
      } with rpc

  let pair_of_currency_info ci =
    ci.currency,
    S.(of_string ci.value_int *
         (match ci.currency with
           | "BTC" -> ~$1
           | "JPY" -> ~$100000
           | _     -> ~$1000))

  type ticker =
      {
        high       : currency_info;
        low        : currency_info;
        avg        : currency_info;
        vwap       : currency_info;
        vol        : currency_info;
        last_local : currency_info;
        last       : currency_info;
        last_orig  : currency_info;
        last_all   : currency_info;
        buy        : currency_info;
        sell       : currency_info
      } with rpc

  let common_ticker_of_ticker t =
    Ticker.make
      ~bid:(S.of_dollar_string t.buy.value_int)
      ~ask:(S.of_dollar_string t.sell.value_int)
      ~high:(S.of_dollar_string t.high.value_int)
      ~low:(S.of_dollar_string t.low.value_int)
      ~vol:(S.of_dollar_string t.vol.value_int)
      ~last:(S.of_dollar_string t.last.value_int) ()

  type wallet =
      {
        balance                : currency_info;
        max_withdraw           : currency_info;
        daily_withdraw_limit   : currency_info;
        monthly_withdraw_limit : currency_info option;
        open_orders            : currency_info;
        operations             : int
      } with rpc
          ("balance" -> "Balance",
           "daily_withdraw_limit" -> "Daily_Withdraw_Limit",
           "max_withdraw" -> "Max_Withdraw",
           "monthly_withdraw_limit" -> "Monthly_Withdraw_Limit",
           "open_orders" -> "Open_Orders",
           "operations" -> "Operations")

  let pair_of_wallet wa =
    pair_of_currency_info wa.balance

  type wallets = (string * wallet) list with rpc

  type private_info =
      {
        created        : string;
        id             : string;
        index          : string;
        language       : string;
        last_login     : string;
        login          : string;
        monthly_volume : currency_info;
        rights         : string list;
        trade_fee      : float;
        wallets        : wallets
      } with rpc
          ("created" -> "Created",
           "id" -> "Id",
           "index" -> "Index",
           "language" -> "Language",
           "last_login" -> "Last_Login",
           "login" -> "Login",
           "monthly_volume" -> "Monthly_Volume",
           "rights" -> "Rights",
           "trade_fee" -> "Trade_Fee",
           "wallets" -> "Wallets")

  let parse_response rpc = let open Rpc in match rpc with
    | Dict ["result", String "success"; "return", obj] -> Lwt.return obj
    | Dict ["result", String "error";
            "error", String err;
            "token", String tok] ->
      raise_lwt Failure (tok ^ ": " ^ err)
    | _ -> raise_lwt Failure "Unknown mtgox response format"

  let pairs_of_private_info pi =
    List.map (fun (_,w) -> pair_of_wallet w) pi.wallets

  let query ?(async=true) ?(params=[]) ?(optfields=[]) endpoint =
    let nonce = Unix.getmicrotime_str () in
    let id = Digest.to_hex (Digest.string nonce) in
    {
      fields=["id", id; "nonce", nonce; "call", endpoint] @ optfields;
      params=if async then params else ("nonce", nonce)::params
    }

  let query_simple curr endpoint = query
    ~optfields:["item", "BTC"; "currency", curr] endpoint

  let json_id_of_query q = List.assoc "id" q.fields

  type depth =
      {
        currency         : string;
        item             : string;
        now              : string;
        price            : string;
        price_int        : string;
        total_volume_int : string;
        type_            : int;
        type_str         : string;
        volume           : string;
        volume_int       : string;
      } with rpc ("type_" -> "type")

  type depth_msg =
      {
        channel  : string;
        depth    : depth;
        op       : string;
        origin   : string;
        private_ : string;
      } with rpc ("private_" -> "private")
end

module Parser = struct
  open Protocol
  let parse_depth (books:Books.t) rpc : Books.t =
    let dm = depth_msg_of_rpc rpc in
    Books.add
      books
      dm.depth.currency
      (Order.kind_of_string dm.depth.type_str)
      S.(of_string dm.depth.price_int * ~$1000)
      S.(of_string dm.depth.total_volume_int)

  let parse_orderbook (books:Books.t) decoder : Books.t =
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
              let price =
                Z.(of_string (List.assoc "price_int" acc |> unstr) * ~$1000)
              and amount = Z.of_string
                (List.assoc "amount_int" acc |> unstr) in
              let books =
                Books.add books "USD" (Order.kind_of_string ctx)
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
      Jsonm.decoder (`String json_str) |> parse_orderbook books
    else
      try
        Jsonrpc.of_string json_str |> parse_depth books
      with e ->
        Lwt.ignore_result $
          Lwt_log.warning_f "Failed to parse automatically: %s\n"
          (Printexc.to_string e);
        (* Automatic parsing failed *)
        Jsonm.decoder (`String json_str) |> parse_orderbook books
end

open Protocol

class mtgox key secret btc_addr push_f =
  let push_msg f content : unit = Websocket.(f $ Some (Frame.of_string content)) in
object (self)
  inherit Exchange.exchange "mtgox" push_f

  val mutable stream = Lwt_stream.of_list [Websocket.(Frame.of_string "")]
  val mutable push   = fun (_:Websocket.Frame.t option) -> ()

  val key               = key
  val buf_json_in       = Buffer.create 4096

  method fee = 0.006
  method currs = StringSet.of_list ["USD"; "EUR"; "GBP"]
  method base_curr = "USD"

  method update =
    let rec update (s, p) =
      stream <- s;
      push   <- p;
      let rec main_loop () =
        let open Websocket in
            lwt frame = Lwt_stream.next s in
            let content = Frame.content frame in
            lwt () = assert_lwt (Frame.opcode frame = `Text) in
            if Frame.final frame then
              (
                Buffer.add_string buf_json_in content;
                main_loop ()
              )
            else
              (
                Buffer.add_string buf_json_in content;
                let buf_str = Buffer.contents buf_json_in in
                (* let () = Printf.printf "%s\n%!" buf_str in *)
                let new_books = Parser.parse books buf_str in
                books <- new_books;
                Buffer.clear buf_json_in; (* maybe use reset here ? *)
                self#notify;
                main_loop ()
              )
        in
      List.iter (push_msg p)
        [unsubscribe Ticker |> rpc_of_async_message |> Jsonrpc.to_string;
         unsubscribe Trade |> rpc_of_async_message |> Jsonrpc.to_string];
      self#command_async (Protocol.query_simple "USD" "depth");
      main_loop ()
    in
    try_lwt
      Websocket.with_connection
        (Uri.of_string "http://websocket.mtgox.com/mtgox") update
    with exc ->
      let exc_str = Printexc.to_string exc in
      Lwt_log.error_f "MtGox websocket error: %s" exc_str
    finally
      Lwt_unix.sleep 1.0 >>= fun () -> self#update

  method command_async query =
    let query_json = Jsonrpc.to_string $ rpc_of_query query in
    let signed_query = CK.hash_string
      (CK.MAC.hmac_sha512 secret) query_json in
    let signed_request64 = Cohttp.Base64.encode
      (key ^ signed_query ^ query_json) in
    Jsonrpc.to_string $ rpc_of_async_message
      ["op", "call";
       "context", "mtgox.com";
       "id", json_id_of_query query;
       "call", signed_request64] |> push_msg push

  method command query =
    let encoded_params = Uri.encoded_of_query $
      List.map (fun (k,v) -> k, [v]) query.params in
    let headers = Cohttp.Header.of_list
      ["User-Agent", "Breakbot";
       "Content-Type", "application/x-www-form-urlencoded";
       "Rest-Key", Uuidm.to_string $ Opt.unbox (Uuidm.of_bytes key);
       "Rest-Sign", Cohttp.Base64.encode $
         CK.hash_string (CK.MAC.hmac_sha512 secret) encoded_params
      ] in
    let endpoint = Uri.of_string $
      "https://mtgox.com/api/1/" ^
      (try
         let item = List.assoc "item" query.fields
         and currency = List.assoc "currency" query.fields in
         item ^ currency
       with Not_found -> "generic")
      ^ "/" ^ (List.assoc "call" query.fields) in
    lwt resp, body = Lwt.bind_opt $
      CoUnix.Client.post ~chunked:false ~headers
      ?body:(CoUnix.Body.body_of_string encoded_params) endpoint in
    CoUnix.Body.string_of_body body >|= Jsonrpc.of_string

  method place_order kind curr price amount =
    lwt rpc = self#command $ Protocol.query ~async:false
      ~optfields:["item","BTC"; "currency", curr]
      ~params:(["type", Order.string_of_kind kind;
                "amount_int", S.to_string amount]
               @ S.(if price > ~$0 then
                   ["price_int", to_string $ price / ~$1000] else []))
      "private/order/add" in
    let rpc_null_filtered = Rpc.filter_null rpc in
    parse_response rpc_null_filtered

  method withdraw_btc amount address =
    lwt rpc = self#command $ Protocol.query ~async:false
      ~params:["address", address; "amount_int", S.to_string amount]
      "bitcoin/send_simple" in
    let rpc_null_filtered = Rpc.filter_null rpc in
    parse_response rpc_null_filtered

  method get_btc_addr = btc_addr

  method get_balances =
    lwt rpc = self#command $ Protocol.query ~async:false "private/info" in
    let rpc_null_filtered = Rpc.filter_null rpc in
    parse_response rpc_null_filtered
    >|= private_info_of_rpc >|= pairs_of_private_info

  method get_ticker curr =
    lwt rpc = self#command $ Protocol.query_simple curr "ticker" in
    let rpc_null_filtered = Rpc.filter_null rpc in
    parse_response rpc_null_filtered >|= ticker_of_rpc

  method get_tickers =
    let currs = StringSet.elements self#currs in
    Lwt_list.map_p (fun c ->
      self#get_ticker c >|= Protocol.common_ticker_of_ticker
                         >|= fun t -> c,t) currs
end
