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
open Common

module CU = Cohttp_lwt_unix
module CB = Cohttp_lwt_body

let period = 2.0

let common_ticker_of_ticker t =
  let open Bitstamp_t in
  Ticker.make
    ~bid:(S.of_face_string t.ticker_bid)
    ~ask:(S.of_face_string t.ticker_ask)
    ~vol:(S.of_face_string t.ticker_volume)
    ~last:(S.of_face_string t.ticker_last)
    ~high:(S.of_face_string t.ticker_high)
    ~low:(S.of_face_string t.ticker_low) ()

class bitstamp userid key secret btc_addr push_f =
  object (self)
    inherit Exchange.exchange "bitstamp" push_f

    method fee = 0.005
    method currs = StringSet.of_list ["USD"]
    method base_curr = "USD"

    method update =
      lwt () =
        try_lwt
          Bitstamp.order_book () >>= function
          | `Error s -> Lwt.fail (Failure s)
          | `Ok ob ->
            let ask_book = List.fold_left
                (fun acc order -> match order with
                   | [p;a] ->
                     let price, amount = (S.of_face_string p), (S.of_face_string a) in
                     Book.add price amount acc
                   | _ -> raise (Invalid_argument "Corrupted bitstamp json or API changed.")
                ) Book.empty ob.Bitstamp_t.order_book_asks
            and bid_book = List.fold_left
                (fun acc order -> match order with
                   | [p;a] ->
                     let price, amount = (S.of_face_string p), (S.of_face_string a) in
                     Book.add price amount acc
                   | _ -> raise (Invalid_argument "Corrupted bitstamp json or API changed.")
                ) Book.empty ob.Bitstamp_t.order_book_bids in
            let () = books <- StringMap.add "USD" (bid_book, ask_book) books in
            Lwt.wrap (fun () -> self#notify)
        with exn ->
          Lwt_log.error_f ~exn "Bitstamp update error"
        finally Lwt_unix.sleep period
      in self#update

method place_order kind curr price amount =
  if curr <> "USD"
  then Lwt.fail (Failure ("Unsupported currency: " ^ curr))
  else
    let price = S.to_face_float price in
    let amount = S.to_face_float amount in
    (match kind with
     | Order.Bid -> Bitstamp.buy ~price ~amount
     | Order.Ask -> Bitstamp.sell ~price ~amount)
    >|= function
    | `Ok _ -> `Ok ()
    | `Error s -> `Error s

method withdraw_btc amount address =
  let amount = S.to_face_float amount in
  Bitstamp.bitcoin_withdrawal ~amount ~address >|= function
  | `Ok _ -> `Ok ()
  | `Error s -> `Error s

method get_btc_addr = btc_addr

method get_balances =
  let open Bitstamp_t in
  Bitstamp.balance () >|= function
  | `Ok b ->
    `Ok ["USD", S.of_face_string b.balance_usd_available;
         "BTC", S.of_face_string b.balance_btc_available]
  | `Error s -> `Error s

method get_ticker _ =
  Bitstamp.ticker () >|= function
  | `Ok ticker -> `Ok (common_ticker_of_ticker ticker)
  | `Error s -> `Error s

method get_tickers =
  Lwt_list.map_p
    (fun c -> self#get_ticker c >>= fun t -> Lwt.return (c,t))
    (StringSet.elements self#currs)
end
