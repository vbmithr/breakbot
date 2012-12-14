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

module Cent = functor (R : (sig val value: float end)) -> struct
  include Z

  let rpc_of_t v = Rpc.String (to_bits v)
  let t_of_rpc = function
    | Rpc.String v -> of_bits v
    | _            -> failwith "Cent.t_of_rpc"
  let of_face_float v = of_float (v *. R.value)
  let to_face_float v = to_float v /. R.value
  let of_face_string v = of_float (float_of_string v *. R.value)
  let to_face_string v = string_of_float (to_face_float v)
end

module S = struct
  include Cent (struct let value = 1e8 end)

  let of_dollar_string v = of_string v * ~$1000
end

module SMap = Map.Make(S)

module Order = struct
  type kind = Bid | Ask

  let kind_of_string str =
    let caseless = String.lowercase str in
    match caseless with
      | "bid" | "bids" -> Bid
      | "ask" | "asks" -> Ask
      | _ -> failwith "Order.kind_of_string"

  let string_of_kind = function Bid -> "bid" | Ask -> "ask"

  type strategy =
    | Market (* non-limit order *)
    | Limit (* Good till canceled -- classic limit order *)
    | Limit_IOC
    (* Immediate or cancel : at least partially executed or immediately
       cancelled *)
    | Limit_FOK (* Fill or kill : fully executed or immediately canceled *)

  type order =
      {
        exchange: string;
        kind: kind;
        currency: string;
        price: S.t;
        amount: S.t
      }

  (** When an exchange fails to send an order *)
  exception Failure of order * string
end

module Ticker = struct
  type t =
    { ts: int64;
      bid: S.t;
      ask: S.t;
      last: S.t;
      vol: S.t;
      high: S.t;
      low: S.t }

  let make ?(ts=Unix.getmicrotime_int64 ())
      ?(high=Z.minus_one)
      ?(low=Z.minus_one)
      ?(vol=Z.minus_one)
      ~bid ~ask ~last () =
    { ts; bid; ask; last; vol; high; low }
end

module type BOOK = sig
  include Map.S with type key = S.t

  type bindings = (S.t * S.t) list

  val rpc_of_t : S.t t -> Rpc.t
  val t_of_rpc : Rpc.t -> S.t t

  val update : S.t -> S.t -> S.t t -> S.t t
  val arbiter : S.t t -> S.t t -> int -> float -> S.t * S.t * S.t * S.t
end

(** A book represent the depth for one currency, and one order kind *)
module Book : BOOK = struct
  include SMap

  type bindings = (S.t * S.t) list with rpc

  let rpc_of_t v = rpc_of_bindings (bindings v)
  let t_of_rpc rpc = of_bindings (bindings_of_rpc rpc)

  let update price amount book =
    try
      let old_amount = SMap.find price book in
      SMap.add price S.(old_amount + amount) book
    with Not_found -> SMap.add price amount book

  let depth_of_ask askbook =
    let open S in
        let _,_,newbook =
          SMap.fold (fun pr am (depth, price, acc) ->
            let ndepth = depth + am
            and nprice = price + pr*am in
            (ndepth, nprice, SMap.add pr (ndepth, nprice) acc)
          ) askbook (~$0, ~$0, SMap.empty)
        in newbook

  let depth_of_bid bidbook =
    let open S in
        let bindings = SMap.bindings bidbook in
        let _,_,newbook =
          List.fold_right (fun (pr, am) (depth, price, acc) ->
            let ndepth = depth + am
            and nprice = price + pr*am in
            (ndepth, nprice, SMap.add pr (ndepth, nprice) acc)
          ) bindings (~$0, ~$0, SMap.empty)
        in newbook

  let amount_at_price_ask askdepthbook price =
    let open S in
        SMap.fold (fun pr (depth, prr) acc ->
          if pr < price then depth else acc) askdepthbook ~$0

  let buy_price book qty =
    let open S in
        SMap.fold (fun pr am (qty_rem, price) ->
          let min_qty = min am qty_rem in
          (qty_rem - min_qty, price + pr*min_qty)
        ) book (qty, ~$0)

  let sell_price book qty =
    let open S in
        let bindings = SMap.bindings book in
        List.fold_right (fun (pr, am) (qty_rem, price) ->
          let min_qty = min am qty_rem in
          (qty_rem - min_qty, price + pr*min_qty)
        ) bindings (qty, ~$0)


  let arbiter bid ask num_iter min_ratio =
    let open S in
        let max_bid = fst $ max_binding bid
        and min_ask = fst $ min_binding ask in
        if gt max_bid min_ask then
          let askdepthbook = depth_of_ask ask in
          let max_qty = amount_at_price_ask askdepthbook max_bid in
          let interval = max_qty / (of_int num_iter) in
          let rec perform acc i =
            if i <= num_iter then
              let qty = (of_int i) * interval in
              let sell_qty_rem, sell_pr = sell_price bid qty
              and buy_qty_rem, buy_pr = buy_price ask qty in
              let gain = sell_pr - buy_pr in
              if max sell_qty_rem buy_qty_rem <> ~$0 then acc
              else
                (Lwt.ignore_result $
                   Lwt_log.debug_f "%f, %f, %f, %f\n%!"
                   (S.to_face_float qty)
                   (S.to_float sell_pr /. 1e16)
                   (S.to_float buy_pr /. 1e16)
                   (S.to_float gain /. 1e16);
                 perform
                   (if (to_float gain /. to_float buy_pr) > min_ratio
                    then Pervasives.max acc (gain, -qty, sell_pr, buy_pr)
                    else acc)
                   (Pervasives.succ i))
            else acc in
          let res, time = Utils.timeit
            (fun () -> perform (~$0, ~$0, ~$0, ~$0) 1) in
          Lwt.ignore_result $
            Lwt_log.info_f ("Computation time: %0.6f\n") time;
          res
        else
          (~$0, ~$0, ~$0, ~$0)
end

module BooksFunctor = struct
  module Make (B : BOOK) = struct
    type book = S.t B.t
    let book_of_rpc = B.t_of_rpc
    let rpc_of_book = B.rpc_of_t

    type t = (book * book) StringMap.t
    type bindings = (string * (book * book)) list with rpc

    let rpc_of_t v = rpc_of_bindings (StringMap.bindings v)
    let t_of_rpc rpc = StringMap.of_bindings (bindings_of_rpc rpc)

    let (empty:t) = StringMap.empty

    let modify
        (action_fun : B.key -> B.key -> S.t B.t -> S.t B.t)
        books curr kind price amount =
      let bid, ask =
        try StringMap.find curr books
        with Not_found -> (B.empty, B.empty)  in
      match kind with
        | Order.Bid ->
          let newbook = action_fun price amount bid in
          StringMap.add curr (newbook, ask) books
        | Order.Ask ->
          let newbook = action_fun price amount ask in
          StringMap.add curr (bid, newbook) books

    let add = modify B.add
    let update = modify B.update

    let remove books curr = StringMap.remove curr books

    let arbiter_unsafe curr books1 books2 nb_iter min_ratio =
      let open S in
          let b1, a1 = StringMap.find curr books1
          and b2, a2 = StringMap.find curr books2 in
          let gain1, qty1, spr1, bpr1 = (B.arbiter b2 a1 nb_iter min_ratio)
          and gain2, qty2, spr2, bpr2 = (B.arbiter b1 a2 nb_iter min_ratio) in
          (gain1 <> ~$0),
          Pervasives.max
            (gain1, -qty1, spr1, bpr1) (gain2, -qty2, spr2, bpr2)

    let print books =
      let print_one book = Book.iter
        (fun price amount -> Printf.printf "(%f,%f) "
          (S.to_face_float price)
          (S.to_face_float amount)) book in
      StringMap.iter (fun curr (bid,ask) ->
        Printf.printf "BID %s\n" curr;
        print_one bid; print_endline "";
        Printf.printf "ASK %s\n" curr;
        print_one ask; print_endline ""
      ) books
  end
end

module Books = BooksFunctor.Make(Book)

module Exchange = struct
  type balances = (string * S.t) list

  class virtual exchange (name:string) (push_f: 'a option -> unit) =
  object (self)
    val mutable books = Books.empty

    method name      = name
    method get_books = books
    method print     = Printf.printf "Books for exchange %s:\n%!" name;
      Books.print books
    method notify = push_f (Some self#name)

    method virtual fee : float
    method virtual currs     : StringSet.t
    method virtual base_curr : string

    method virtual update    : unit Lwt.t
    method virtual place_order : Order.kind -> string -> S.t -> S.t ->
      Rpc.t Lwt.t
    method virtual withdraw_btc : S.t -> string -> Rpc.t Lwt.t
    method virtual get_btc_addr : string
    method virtual get_balances : balances Lwt.t
    method virtual get_tickers  : (string * Ticker.t) list Lwt.t
  end
end
