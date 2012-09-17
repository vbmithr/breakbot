open Utils

module Currency = struct
  type t   = GBP | EUR | USD | PLN

  let to_string = function
    | GBP -> "GBP"
    | EUR -> "EUR"
    | USD -> "USD"
    | PLN -> "PLN"

  let of_string = function
    | "GBP" -> GBP
    | "EUR" -> EUR
    | "USD" -> USD
    | "PLN" -> PLN
    | _ -> failwith "Currency.of_string"
end

module Satoshi = struct
  type t = int

  let of_btc_float v = int_of_float (v *. 1e8)
  let to_btc_float v = float_of_int v /. 1e8

  let of_string = int_of_string
  let of_btc_string v = int_of_float (float_of_string v *. 1e8)
end

module Order = struct
  type kind = Bid | Ask
  type strategy =
    | Market
    | Good_till_cancelled
    | Immediate_or_cancel
    | Fill_or_kill

  type t =
      {
        direction : kind;
        stategy   : strategy;
        currency  : Currency.t;
        price     : int; (* price in satoshis *)
        amount    : int (* amount of BTC in satoshis *)
      }
end

module Book = struct
  (** A book represent the depth for one currency, and one order
      kind *)
  type t = int IntMap.t

  let (empty:t) = IntMap.empty

  let make_books_fun () =
    let books = Hashtbl.create 10 in
    let add (curr:Currency.t) price amount =
      let book =
        try Hashtbl.find books curr
        with Not_found -> empty in
      let new_book =
        if IntMap.mem price book then
          let old_amount = IntMap.find price book in
          IntMap.add price (old_amount + amount) book
        else IntMap.add price amount book
      in Hashtbl.replace books curr new_book in
    let print () =
      let print_one book = IntMap.iter
        (fun rate amount -> Printf.printf "(%f,%f) "
          (Satoshi.to_btc_float rate)
          (Satoshi.to_btc_float amount)) book in
      Hashtbl.iter (fun curr book ->
        Printf.printf "Currency: %s\n" (Currency.to_string curr);
        print_one book; print_endline "";

      ) books
    in (add, print)

  let add_to_bid_books, print_bid_books = make_books_fun ()
  let add_to_ask_books, print_ask_books = make_books_fun ()
end
