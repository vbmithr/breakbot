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

let api_url = Uri.of_string "https://btc-e.com/tapi"
let period = 2.0

module Protocol = struct
  let btcecurr_of_curr = function
    | "EUR" -> "eur"
    | "USD" -> "usd"
    | "RUB" -> "rur"
    | _     -> failwith "btcecurr_of_curr"

  let make_get_url ?(obj="btc") curr kind =
    let btce_curr = btcecurr_of_curr curr in
    Uri.of_string @@
      "https://btc-e.com/api/2/" ^ obj ^ "_" ^ btce_curr ^ "/" ^ kind

  type ticker_ =
    {
      high: float;
      low: float;
      avg: float;
      vol: float;
      vol_cur: float;
      last: float;
      buy: float;
      sell: float;
      server_time: float
    } with rpc

  type ticker = { ticker: ticker_ } with rpc

  let common_ticker_of_ticker t =
    Ticker.make
      ~ts:(Int64.of_float t.ticker.server_time)
      ~bid:(S.of_face_float t.ticker.sell)
      ~ask:(S.of_face_float t.ticker.buy)
      ~vol:(S.of_face_float t.ticker.vol_cur)
      ~last:(S.of_face_float t.ticker.last)
      ~high:(S.of_face_float t.ticker.high)
      ~low:(S.of_face_float t.ticker.low) ()

  type funds =
    {
      usd: float; rur: float; eur: float;
      btc: float; ltc: float; nmc: float
    } with rpc

  type rights =
    {
      info: float;
      trade: float;
      withdraw: float
    } with rpc

  type getinfo =
    {
      funds: funds;
      rights: rights;
      transaction_count: float;
      open_orders: float;
      server_time: float
    } with rpc

  let query name params =
    let nonce = Unix.gettimeofday_str () in
    let params = ["method", name; "nonce", nonce] @ params in
    Uri.encoded_of_query @@ List.map (fun (k,v) -> k,[v]) params

  let parse_response rpc = let open Rpc in match rpc with
    | Dict ["success", Int 1L; "return", obj] -> Lwt.return obj
    | Dict ["success", Int 0L; "error", String err] -> raise_lwt (Failure err)
    | _ -> raise_lwt (Failure "should not happen")
end

class btce key secret btc_addr push_f =
  object (self)
    inherit Exchange.exchange "btce" push_f

    method fee = 0.002
    method currs = StringSet.of_list ["USD"]
    method base_curr = "USD"

    method update =
      let open Protocol in
      lwt () =
        try_lwt
          CU.Client.get (Protocol.make_get_url "USD" "depth") >>= function
          | None -> Lwt.fail (Failure "CU.Client.get returned None")
          | Some (response, body) ->
            CB.string_of_body body >>= fun body ->
            let open Btce_j in
            let order_book = order_book_of_string body in
            let ask_book = List.fold_left
                (fun acc order -> match order with
                   | [p;a] ->
                     let price, amount = (S.of_face_float p), (S.of_face_float a) in
                     Book.add price amount acc
                   | _ -> raise (Invalid_argument "Corrupted btce json or API changed.")
                ) Book.empty order_book.asks
            and bid_book = List.fold_left
                (fun acc order -> match order with
                   | [p;a] ->
                     let price, amount = (S.of_face_float p), (S.of_face_float a) in
                     Book.add price amount acc
                   | _ -> raise (Invalid_argument "Corrupted btce json or API changed.")
                ) Book.empty order_book.bids in
            let () = books <- StringMap.add "USD" (bid_book, ask_book) books in
            Lwt.wrap (fun () -> self#notify)
        with exn -> Lwt_log.error ~exn "Btce update error"
finally Lwt_unix.sleep period
in self#update


method command query =
  let signed_query =
    CK.hash_string (CK.MAC.hmac_sha512 secret) query in
  let signed_query_hex =
    CK.transform_string (CK.Hexa.encode ()) signed_query in
  let headers = Cohttp.Header.of_list
      [
        "User-Agent", "Breakbot";
        "Content-Type", "application/x-www-form-urlencoded";
        "Key", key;
        "Sign", signed_query_hex
      ] in
  lwt resp, body = Lwt.bind_opt @@
                     CU.Client.post ~chunked:false ~headers
                       ?body:(CB.body_of_string query) api_url in
  CB.string_of_body body
  >|= Jsonrpc.of_string >>= Protocol.parse_response

method place_order kind curr price amount =
  let pair = "btc_" ^ Protocol.btcecurr_of_curr curr in
  let kind = match kind with Order.Bid -> "buy" | Order.Ask -> "sell" in
  self#command @@ Protocol.query "Trade"
      [
        "pair", pair;
        "type", kind;
        "rate", S.to_face_string price;
        "amount", S.to_face_string amount
      ]

method withdraw_btc amount address =
  raise_lwt Failure "Not supported by the exchange"

method get_btc_addr = btc_addr

method get_balances =
  let open Protocol in
  lwt rpc = self#command @@ query "getInfo" [] in
  let rpc_float = Rpc.int_to_float rpc in
  let getinfo = getinfo_of_rpc rpc_float in
  Lwt.return
    ["EUR", S.of_face_float getinfo.funds.eur;
     "USD", S.of_face_float getinfo.funds.usd;
     "RUB", S.of_face_float getinfo.funds.rur;
     "BTC", S.of_face_float getinfo.funds.btc]

method get_ticker curr =
  lwt rpc = (Lwt.wrap2 Protocol.make_get_url curr "ticker")
    >>= Jsonrpc.get_int_to_float in
  Lwt.wrap (fun () ->
      Protocol.common_ticker_of_ticker @@ Protocol.ticker_of_rpc rpc)

method get_tickers =
  Lwt_list.map_p
    (fun c -> lwt t = self#get_ticker c in Lwt.return (c,t))
    (StringSet.elements self#currs)
end
