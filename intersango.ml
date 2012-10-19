open Utils
open Lwt_utils
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
          let kind = Opt.unopt kind in
          let curr = Opt.unopt curr in
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
        | `Lexeme (`String s) -> let (name:string) = Opt.unopt name
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

  let rec parse_accounts accounts_str =
    try
      account_list_of_rpc (Jsonrpc.of_string accounts_str)
    with
      | Rpc.Runtime_error (str, t) ->
        Printf.printf "%s\n" (Jsonrpc.to_string t); exit 1
end

class intersango api_key =
  let streaming_uri = "db.intersango.com"
  and streaming_port = "1337"
  and api_uri = "https://intersango.com/api/authenticated/v0.1/" in
  let list_accounts_uri = api_uri ^ "listAccounts.php"
  and order_uri = api_uri ^ "placeLimitOrder.php"
  and withdraw_uri = api_uri ^ "createBitcoinWithdrawalRequest.php" in
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
    in Lwt_io.with_connection_dns streaming_uri streaming_port  update

  method currs = stringset_of_list ["GBP"; "EUR"; "USD"; "PLN"]

  method base_curr = "GBP"

  method place_order kind curr price amount =
    lwt base_account_id = self#get_account_id "BTC"
    and quote_account_id = self#get_account_id curr in
    let params = Cohttp.Header.of_list
      ["api_key", api_key;
       "quantity", S.to_face_string amount;
       "rate", S.to_face_string price;
       "selling", (match kind with
         | Order.Ask -> "true"
         | Order.Bid -> "false");
       "base_account_id", base_account_id;
       "quote_account_id", quote_account_id;
       "type", "fok"] in
    let uri = Uri.of_string order_uri in
    lwt ret = CoUnix.Client.post_form ~params uri in
    let _, body = Opt.unopt ret in
    lwt body_string = CoUnix.Body.string_of_body body in
    Lwt.return (Printf.printf "Place_order: %s\n%!" body_string)

  method private get_account_id curr =
    lwt a =
      List.find
        (fun accnt -> accnt.Parser.currency_abbreviation = curr)
      =|< accounts
    in Lwt.return (Int64.to_string a.Parser.id)

  method withdraw_btc amount address =
    lwt account_id = self#get_account_id "BTC" in
    let params = Cohttp.Header.of_list
      ["api_key", api_key;
       "amount", S.to_face_string amount;
       "address", address;
       "account_id", account_id
      ] in
    let uri = Uri.of_string withdraw_uri in
    lwt ret = CoUnix.Client.post_form ~params uri in
    let _, body = Opt.unopt ret in
    lwt body_string = CoUnix.Body.string_of_body body in
    Lwt.return (Printf.printf "Withdraw BTC: %s\n%!" body_string)

  method get_balances =
    accounts >|= List.map (fun ac ->
      Parser.(ac.currency_abbreviation, S.of_face_string ac.balance))

  initializer
    accounts <-
      lwt ret = CoUnix.Client.post_form
        ~params:(Cohttp.Header.init_with "api_key" api_key)
        (Uri.of_string list_accounts_uri) in
      let (_:CoUnix.Response.t),body = Opt.unopt ret in
      lwt body_str = CoUnix.Body.string_of_body body in
      Lwt.return (Parser.parse_accounts body_str)
end
