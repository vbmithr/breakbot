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

let period = 2.0

let make_uri endpoint =
  Uri.of_string $ "https://www.bitstamp.net/api/" ^ endpoint ^ "/"

module Protocol = struct
  type depth =
      { bids: (float * float) list;
        asks: (float * float) list
      } with rpc

  type ticker =
      {
        high: float;
        low: float;
        volume: float;
        last: float;
        bid: float;
        ask: float;
      } with rpc

  let common_ticker_of_ticker t =
    Ticker.make
      ~bid:(S.of_face_float t.bid)
      ~ask:(S.of_face_float t.ask)
      ~vol:(S.of_face_float t.volume)
      ~last:(S.of_face_float t.last)
      ~high:(S.of_face_float t.high)
      ~low:(S.of_face_float t.low) ()

  type balance =
      {
        usd_balance: float;
        btc_balance: float;
        usd_reserved: float;
        btc_reserved: float;
        usd_available: float;
        btc_available: float;
        fee: float
      } with rpc

  let parse_response rpc = let open Rpc in match rpc with
    | Dict ["error", rpc] -> failwith (Jsonrpc.to_string rpc)
    | oth -> oth
end

class bitstamp login passwd btc_addr push_f=
object (self)
  inherit Exchange.exchange "bitstamp" push_f

  method fee = 0.005
  method currs = StringSet.of_list ["USD"]
  method base_curr = "USD"

  method update =
    let open Protocol in
        lwt () =
          try_lwt
            lwt rpc = Jsonrpc.get_int_to_float $ make_uri "order_book" in
            let depth = depth_of_rpc rpc in
            let ask_book = List.fold_left
              (fun acc d -> let price_float, amount_float = d in
                            let price, amount =
                              (S.of_face_float price_float),
                              (S.of_face_float amount_float) in
                            Book.add price amount acc) Book.empty depth.asks
            and bid_book = List.fold_left
              (fun acc d -> let price_float, amount_float = d in
                          let price, amount =
                            (S.of_face_float price_float),
                            (S.of_face_float amount_float) in
                          Book.add price amount acc) Book.empty depth.bids in
            let () = books <- StringMap.add "USD" (bid_book, ask_book) books in
            Lwt.wrap (fun () -> self#notify)
          with exc ->
            let exc_str = Printexc.to_string exc in
            Lwt_log.error_f "Bitstamp update error: %s" exc_str
          finally Lwt_unix.sleep period
        in self#update

  method command endpoint params =
    let headers = Cohttp.Header.of_list
      [
        "User-Agent", "Breakbot";
        "Content-Type", "application/x-www-form-urlencoded";
      ] in
    let params = ["user", login; "password", passwd] @ params in
    let body = Uri.encoded_of_query $ List.map (fun (k,v) -> k,[v]) params
      |> CoUnix.Body.body_of_string
    in
    lwt resp, body = Lwt.bind_opt $
      CoUnix.Client.post ~chunked:false ~headers
      ?body (make_uri endpoint) in
      CoUnix.Body.string_of_body body >|= Jsonrpc.of_string

  method place_order kind curr price amount =
    if curr <> "USD"
    then raise_lwt (Failure ("Unsupported currency: " ^ curr))
    else
      lwt rpc =
        self#command
          (match kind with Order.Bid -> "buy" | Order.Ask -> "sell")
          ["price", S.to_face_string price;
           "amount", S.to_face_string amount]
      in Lwt.wrap1 Protocol.parse_response rpc

  method withdraw_btc amount address =
    lwt rpc =
      self#command "bitcoin_withdrawal"
        ["amount", S.to_face_string amount;
         "address", address] in Lwt.wrap1 Protocol.parse_response rpc

  method get_btc_addr = btc_addr

  method get_balances =
    let open Protocol in
    lwt rpc = self#command "balance" [] in
    let rpc_float = Rpc.int_to_float rpc in
    let balance = balance_of_rpc rpc_float in
    Lwt.return
      ["USD", S.of_face_float balance.usd_available;
       "BTC", S.of_face_float balance.btc_available]

  method get_ticker _ =
    Jsonrpc.get_int_to_float $ make_uri "ticker" >|=
    Protocol.ticker_of_rpc >|= Protocol.common_ticker_of_ticker

  method get_tickers =
    Lwt_list.map_p
      (fun c -> lwt t = self#get_ticker c in Lwt.return (c,t))
      (StringSet.elements self#currs)
end
