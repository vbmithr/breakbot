open Utils
open Lwt_utils
open Jsonrpc_utils
open Common

module CoUnix = Cohttp_lwt_unix

module Currency = struct
  let to_int = function
    | "GBP" -> 1
    | "EUR" -> 2
    | "USD" -> 3
    | "PLN" -> 4
    | _ -> failwith "Currency.to_int"

  let of_int = function
    | 1 -> "GBP"
    | 2 -> "EUR"
    | 3 -> "USD"
    | 4 -> "PLN"
    | _ -> failwith "Currency.of_int"

  let of_string = function
    | "1" -> "GBP"
    | "2" -> "EUR"
    | "3" -> "USD"
    | "4" -> "PLN"
    | _ -> failwith "Currency.of_int"
end

module Parser = struct
  let parse_orderbook books decoder =
    let rec parse_orderbook ?curr ?kind ?price books =
      match Jsonm.decode decoder with
        | `Lexeme (`Name "bids") ->
          parse_orderbook ?price ?curr ~kind:Order.Bid books
        | `Lexeme (`Name "asks") ->
          parse_orderbook ?price ?curr ~kind:Order.Ask books
        | `Lexeme (`Name s) when String.is_int s ->
          let curr = Currency.of_int (int_of_string s)
          in parse_orderbook ?kind ?price ~curr books
        | `Lexeme (`Name s) when String.is_float s ->
          let price = S.of_face_string s in
          let amount = match Jsonm.decode decoder with
            | `Lexeme (`String am) -> S.of_face_string am
            | _ -> failwith "Intersango probably changed its format" in
          let kind = Opt.unbox kind in
          let curr = Opt.unbox curr in
          let books = Books.update books curr kind price amount in
          parse_orderbook ~curr ~kind books
        | `Lexeme `Ae -> books
        | `Lexeme l -> parse_orderbook ?curr ?kind ?price books
        | `Error e -> Jsonm.pp_error Format.err_formatter e;
          failwith "Intersango.Parser.parse_orderbook"
        | `Await -> Printf.printf "Awaiting...\n%!"; books
        | `End -> Printf.printf "End...\n%!"; books
    in parse_orderbook books

  let parse_depth books decoder =
    let rec parse_depth ?name acc =
      match Jsonm.decode decoder with
        | `Lexeme (`Name s) -> parse_depth ~name:s acc
        | `Lexeme (`String s) -> let (name:string) = Opt.unbox name
                      in parse_depth ((name, s)::acc)
        | `Lexeme `Ae ->
          let kind = Order.kind_of_string (List.assoc "type" acc) in
          let curr = Currency.of_int
            (int_of_string ((List.assoc "currency_pair_id" acc))) in
          let price = S.of_face_string (List.assoc "rate" acc) in
          let amount = S.of_face_string (List.assoc "amount" acc) in
          Books.update books curr kind price amount
        | `Lexeme _ -> parse_depth acc
        | `Error e -> Jsonm.pp_error Format.err_formatter e;
          failwith "Intersango.Parser.parse_detph"
        | `Await -> Printf.printf "Awaiting...\n%!"; books
        | `End -> Printf.printf "End...\n%!"; books

    in parse_depth []

  let rec parse_jsonm books decoder =
    match Jsonm.decode decoder with
      | `Lexeme (`String "orderbook") -> parse_orderbook books decoder
      | `Lexeme (`String "depth")     -> parse_depth books decoder
      | `Lexeme _                     -> parse_jsonm books decoder
      | `Error e -> Jsonm.pp_error Format.err_formatter e;
        failwith "Intersango.Parser.parse_jsonm"
      | `Await -> Printf.printf "Awaiting...\n%!"; books
      | `End -> books

  type error = { error: string } with rpc

  let parse_response rpc = let open Rpc in match rpc with
    | Dict ["error", String msg] -> raise_lwt Failure msg
    | obj -> Lwt.return obj

  type account =
      {
        id                           : int64;
        balance                      : string;
        currency_id                  : int;
        currency_abbreviation        : string;
        account_trade_fee_percentage : string;
        reference_code               : string option;
        liabilities                  : string;
        locked_address               : bool option;
        bitcoin_address              : string option
      } with rpc

  type account_list = account list with rpc

  let account_id_of_curr accs curr =
    let curr_int = Currency.to_int curr in
    let account = List.find (fun acc -> acc.currency_id = curr_int) accs in
    account.id

  let rec parse_accounts accounts_str =
    try
      account_list_of_rpc (Jsonrpc.of_string accounts_str)
    with
      | Rpc.Runtime_error (str, t) ->
        Printf.printf "%s\n" (Jsonrpc.to_string t); exit 1

  type ticker =
      { last: string; vol:string option; buy:string; sell: string }
  with rpc

  type tickers = (string * ticker) list with rpc

  let common_ticker_of_ticker t =
    Ticker.make
      ?vol:(Opt.map S.of_face_string t.vol)
      ~bid:(S.of_face_string t.buy)
      ~ask:(S.of_face_string t.sell)
      ~last:(S.of_face_string t.last) ()
end

class intersango api_key =
  let streaming_uri = "db.intersango.com"
  and streaming_port = "1337"
  and api_uri = "https://intersango.com/api/authenticated/v0.1/" in
  let list_accounts_uri = Uri.of_string $ api_uri ^ "listAccounts.php"
  and order_uri = Uri.of_string $ api_uri ^ "placeLimitOrder.php"
  and withdraw_uri = Uri.of_string $
    api_uri ^ "createBitcoinWithdrawalRequest.php"
  and ticker_uri = Uri.of_string "https://intersango.com/api/ticker.php" in
  (* and trades_uri = Uri.of_string "https://intersango.com/api/trades.php" *)
  (* and depth_uri  = Uri.of_string "https://intersango.com/api/depth.php" in *)
object (self)
  inherit Exchange.exchange "intersango"

  val mutable accounts = Lwt.return []

  method update =
    let rec update (ic, oc) =
      lwt line = Lwt_io.read_line ic in
      let () = Printf.printf "%s\n%!" line in
      let decoder = Jsonm.decoder (`String line) in
      let () = books <- Parser.parse_jsonm books decoder in
      lwt () = self#notify in
      update (ic, oc)
    in try_lwt
         Lwt_io.with_connection_dns streaming_uri streaming_port update
      with Not_found ->
        let () = Printf.printf
          "Intersango#update failed. Retrying in 60 seconds.\n%!" in
        lwt () = Lwt_unix.sleep 60.0 in self#update


  method currs = StringSet.of_list ["GBP"; "EUR"; "PLN"]

  method base_curr = "GBP"

  method get_accounts =
    if accounts = Lwt.return [] then
      (accounts <-
         (lwt resp, body = Lwt.bind_opt $ CoUnix.Client.post_form
            ~params:(Cohttp.Header.init_with "api_key" api_key)
            list_accounts_uri in
          lwt body_str = CoUnix.Body.string_of_body body in
          Lwt.wrap (fun () -> Parser.parse_accounts body_str));
       accounts)
    else accounts

  method place_order kind curr price amount =
    lwt accounts = self#get_accounts in
    let base_account_id = Parser.account_id_of_curr accounts "BTC"
    and quote_account_id = Parser.account_id_of_curr accounts curr in
    let params = Cohttp.Header.of_list
      ["api_key", api_key;
       "quantity", S.to_face_string amount;
       "rate", S.to_face_string price;
       "selling", (match kind with
         | Order.Ask -> "true"
         | Order.Bid -> "false");
       "base_account_id", Int64.to_string base_account_id;
       "quote_account_id", Int64.to_string quote_account_id;
       "type", "fok"] in
    lwt resp, body = Lwt.bind_opt $
      CoUnix.Client.post_form ~params order_uri in
    lwt body_str = CoUnix.Body.string_of_body body in
    let rpc = Jsonrpc.of_string body_str in
    let rpc_filter_null = Rpc.filter_null rpc in
    Parser.parse_response rpc_filter_null

  method withdraw_btc amount address =
    lwt accounts = self#get_accounts in
    let account_id = Parser.account_id_of_curr accounts "BTC" in
    let params = Cohttp.Header.of_list
      ["api_key", api_key;
       "amount", S.to_face_string amount;
       "address", address;
       "account_id", Int64.to_string account_id
      ] in
    lwt resp, body = Lwt.bind_opt $
      CoUnix.Client.post_form ~params withdraw_uri in
    lwt body_str = CoUnix.Body.string_of_body body in
    let rpc = Jsonrpc.of_string body_str in
    let rpc_filter_null = Rpc.filter_null rpc in
    Parser.parse_response rpc_filter_null

  method get_balances =
    self#get_accounts >|= List.map (fun ac ->
      Parser.(ac.currency_abbreviation, S.of_face_string ac.balance))

  method get_tickers =
    lwt rpc = Jsonrpc.get_filter_null ticker_uri in
    let tickers = Parser.tickers_of_rpc rpc in
    Lwt.wrap2 List.map
      (fun (c,v) -> Currency.of_string c,
        Parser.common_ticker_of_ticker v) tickers
end
