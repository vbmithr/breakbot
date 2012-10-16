open Utils
open Common

module CoUnix = Cohttp_lwt_unix

let post_form ?headers ~params uri =
  let open Cohttp in
  let headers = Header.add_opt headers "content-type" "application/x-www-form-urlencoded" in
  let q = List.map (fun (k,v) -> k, [v]) (Header.to_list params) in
  let body = CoUnix.Body.body_of_string (Uri.encoded_of_query q) in
  lwt body_len, body = CoUnix.Body.get_length body in
  let headers = Header.add headers "content-length" (string_of_int body_len) in
  CoUnix.Client.post ~headers ?body uri

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
          let price = Dollar.of_face_string s in
          let amount = match Jsonm.decode decoder with
            | `Lexeme (`String am) -> Satoshi.of_face_string am
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
          let price = Dollar.of_face_string (List.assoc "rate" acc) in
          let amount = Satoshi.of_face_string (List.assoc "amount" acc) in
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

  method private get_account_id curr =
    lwt a =
      List.find
        (fun accnt -> accnt.Parser.currency_abbreviation = curr)
      =|< accounts
    in Lwt.return (Int64.to_string a.Parser.id)

  method update =
    let rec update (ic, oc) =
      lwt line = Lwt_io.read_line ic in
      let () = Printf.printf "%s\n%!" line in
      let decoder = Jsonm.decoder (`String line) in
      let () = books <- Parser.parse_jsonm books decoder in
      lwt () = Z.(self#place_order Order.Bid "GBP" ~$100000 ~$100000000) in
      lwt () = Z.(self#withdraw_btc ~$100000000 "1AD2Fo5SAnmuPyH2JtkeFkEJoL2mRMwaZi") in
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
       "quantity", Satoshi.to_face_string amount;
       "rate", Dollar.to_face_string price;
       "selling", (match kind with
         | Order.Ask -> "true"
         | Order.Bid -> "false");
       "base_account_id", base_account_id;
       "quote_account_id", quote_account_id;
       "type", "fok"] in
    let uri = Uri.of_string order_uri in
    lwt ret = post_form ~params uri in
    let _, body = Opt.unopt ret in
    lwt body_string = CoUnix.Body.string_of_body body in
    Lwt.return (Printf.printf "Place_order: %s\n%!" body_string)

  method private withdraw_btc amount address =
    lwt account_id = self#get_account_id "BTC" in
    let params = Cohttp.Header.of_list
      ["api_key", api_key;
       "amount", Satoshi.to_face_string amount;
       "address", address;
       "account_id", account_id
      ] in
    let uri = Uri.of_string withdraw_uri in
    lwt ret = post_form ~params uri in
    let _, body = Opt.unopt ret in
    lwt body_string = CoUnix.Body.string_of_body body in
    Lwt.return (Printf.printf "Withdraw BTC: %s\n%!" body_string)

  initializer
    accounts <-
      lwt ret = post_form
        ~params:(Cohttp.Header.init_with "api_key" api_key)
        (Uri.of_string list_accounts_uri) in
      let (_:CoUnix.Response.t),body = Opt.unopt ret in
      lwt body_str = CoUnix.Body.string_of_body body in
      let () = Printf.printf "%s\n%!" body_str in
      Lwt.return (Parser.parse_accounts body_str)
end
