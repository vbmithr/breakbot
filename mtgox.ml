open Utils
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
      let fields, params = List.partition (fun(k,v) -> k <> "params") fields in
      {
        fields=params_of_rpc $ Rpc.Dict fields;
        params=params_of_rpc $ Rpc.Dict params
      }
    | _ -> failwith "query_of_rpc"

  type currency_info =
      {
        currency: string;
        display: string;
        display_short: string;
        value: string;
        value_int: string
      } with rpc

  let pair_of_currency_info ci =
    ci.currency,
    S.(of_string ci.value_int *
         (match ci.currency with
           | "BTC" -> ~$1
           | "JPY" -> ~$100000
           | _     -> ~$1000))

  type wallet =
      {
        balance: currency_info;
        max_withdraw: currency_info;
        daily_withdraw_limit: currency_info;
        monthly_withdraw_limit: currency_info option;
        open_orders: currency_info;
        operations: int
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
        created: string;
        id: string;
        index: string;
        language: string;
        last_login: string;
        login: string;
        monthly_volume: currency_info;
        rights: string list;
        trade_fee: float;
        wallets: wallets
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

  type result_pi = {result: string; return: private_info} with rpc

  let get_private_info rpc =
    let res = result_pi_of_rpc rpc in res.return

  let pairs_of_private_info pi =
    List.map (fun (_,w) -> pair_of_wallet w) pi.wallets

  type result_type =
    [ `Async | `Sync of string ]

  let query ?(async=true) ?(params=[]) ?(optfields=[]) endpoint =
    let nonce = Unix.getmicrotime_str () in
    let id = Digest.to_hex (Digest.string nonce) in
    {
      fields=["id", id; "nonce", nonce; "call", endpoint] @ optfields;
      params=if async then params else ("nonce", nonce)::params
    }

  let get_depth = query
    ~optfields:["item", "BTC"; "currency", "USD"] "depth"

  let get_ticker = query
    ~optfields:["item", "BTC"; "currency", "USD"] "ticker"

  let json_id_of_query q = List.assoc "id" q.fields

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
          curr: string;
          kind: Order.kind;
          price: Z.t;
          amount: Z.t;
          ts: int64
        }

    let of_depth (d:LL.depth_msg) =
      let d = d.LL.depth in
      {
        curr=d.LL.currency;
        kind=Order.kind_of_string d.LL.type_str;
        price=Z.(of_string d.LL.price_int * ~$1000);
        amount=Z.of_string d.LL.total_volume_int;
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
              and price =
                Z.(of_string (List.assoc "price_int" acc |> unstr) * ~$1000)
              and amount = Z.of_string
                (List.assoc "amount_int" acc |> unstr) in
              let books =
                Books.add ~ts books "USD" (Order.kind_of_string ctx)
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

  val mutable buf_in    = Sharedbuf.empty ~bufsize:0 ()
  val mutable buf_out   = Sharedbuf.empty ~bufsize:0 ()

  val key               = key
  val buf_json_in       = Buffer.create 4096

  method update =
    let rec update (bi, bo) =
      buf_in    <- bi;
      buf_out   <- bo;

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
                let () = Buffer.clear buf_json_in in (* maybe use reset here ? *)
                self#notify)
          in Lwt.return len)
        in
        main_loop () in

      lwt () = Sharedbuf.write_lines buf_out
        [unsubscribe Ticker |> rpc_of_async_message |> Jsonrpc.to_string;
         unsubscribe Trade |> rpc_of_async_message |> Jsonrpc.to_string] in
      lwt _ = self#command (Protocol.query "private/info") in
      (* lwt (_:int) = self#command (Protocol.get_depth) in *)
      (* lwt (_:int) = self#command (Protocol.get_currency_info) in *)

      main_loop () in
    Websocket.with_websocket "http://websocket.mtgox.com/mtgox" update

  method private command ?(async=true) query =
    let query_json = Jsonrpc.to_string $ rpc_of_query query in
    let signed_query = CK.hash_string (CK.MAC.hmac_sha512 secret) query_json in
    let signed_request64 = Cohttp.Base64.encode
      (key ^ signed_query ^ query_json) in
    if async then
      let res = Jsonrpc.to_string $ rpc_of_async_message
        ["op", "call";
         "context", "mtgox.com";
         "id", json_id_of_query query;
         "call", signed_request64] in
      lwt (_:int) = Sharedbuf.write_line buf_out res in
      Lwt.return `Async
    else
      let encoded_params = Uri.encoded_of_query $
        List.map (fun (k,v) -> k, [v]) query.params in
      let headers = Cohttp.Header.of_list
        ["User-Agent", "GoxCLI";
         "Content-Type", "application/x-www-form-urlencoded";
         "Rest-Key", Uuidm.to_string $ Opt.unopt (Uuidm.of_bytes key);
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
      lwt ret = CoUnix.Client.post ~chunked:false ~headers
       ?body:(CoUnix.Body.body_of_string encoded_params) endpoint in
      let _, body = Opt.unopt ret in
      lwt body_string = CoUnix.Body.string_of_body body in
      Lwt.return $ `Sync body_string

  method currs = stringset_of_list
    ["USD"; "AUD"; "CAD"; "CHF"; "CNY"; "DKK"; "EUR"; "GBP";
     "HKD"; "JPY"; "NZD"; "PLN"; "RUB"; "SEK"; "SGD"; "THB"]

  method base_curr = "USD"

  method place_order kind curr price amount =
    lwt res = self#command ~async:false $
      Protocol.query ~async:false
      ~optfields:["item","BTC"; "currency", curr]
      ~params:(["type", Order.string_of_kind kind;
               "amount_int", S.to_string amount]
      @ S.(if price > ~$0 then
          ["price_int", to_string $ price / ~$1000] else []))
      "private/order/add" in
    match res with
      | `Sync str -> Printf.printf "%s\n%!" str; Lwt.return ()
      | _ -> failwith "place_order"

  method withdraw_btc amount address =
    lwt res = self#command ~async:false $
      Protocol.query ~async:false
      ~params:["address", address; "amount_int", S.to_string amount]
      "bitcoin/send_simple" in
    match res with
      | `Sync str -> Printf.printf "%s\n%!" str; Lwt.return ()
      | _ -> failwith "withdraw_btc"

  method get_balances =
    lwt res = self#command ~async:false $
      Protocol.query ~async:false "private/info" in
    match res with
      | `Sync str ->
        Jsonrpc.of_string_filter_null str |> get_private_info |>
            pairs_of_private_info |> Lwt.return
      | _ -> failwith "get_balances"

end
