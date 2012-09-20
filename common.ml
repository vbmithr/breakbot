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

  let kind_of_string str =
    let caseless = String.lowercase str in
    match caseless with
      | "bid" | "bids" -> Bid
      | "ask" | "asks" -> Ask
      | _ -> failwith "Order.kind_of_string"

end

(** A book represent the depth for one currency, and one order kind *)
module Book = struct
  type t = int IntMap.t

  let (empty:t) = IntMap.empty

  let add book price amount =
    IntMap.add price amount book

  let update book price amount =
    try
      let old_amount = IntMap.find price book in
      IntMap.add price (old_amount + amount) book
    with Not_found -> IntMap.add price amount book

  let amount_below_or_eq book price =
    let l, data, r = IntMap.split price book in
    IntMap.fold (fun pr am acc -> acc + am)
      l (Opt.unopt ~default:0 data)

  let amount_above_or_eq book price =
    let l, data, r = IntMap.split price book in
    IntMap.fold (fun pr am acc -> acc + am)
      r (Opt.unopt ~default:0 data)

  let diff book1 book2 =
    let merge_fun key v1 v2 = match v1, v2 with
      | None, None       -> failwith "Should never happen"
      | Some v1, None    -> Some (-v1)
      | None, Some v2    -> Some v2
      | Some v1, Some v2 -> Some (v2-v1) in
    IntMap.merge merge_fun book1 book2

  let patch book patch =
    let merge_fun key v1 v2 = match v1, v2 with
      | None, None -> failwith "Should never happen"
      | Some v1, None -> Some v1
      | None, Some v2 -> Some v2
      | Some v1, Some v2 -> Some (v1+v2) in
    IntMap.merge merge_fun book patch
end

(** Books are a set of books, one for each currency *)
module Books = struct
  type t = (Currency.t, int IntMap.t) Hashtbl.t

  let empty () = Hashtbl.create 10

  let add books (curr:Currency.t) price amount =
    let book =
      try Hashtbl.find books curr
      with Not_found -> Book.empty in
    let new_book = Book.add book price amount in
    Hashtbl.replace books curr new_book

  let update books (curr:Currency.t) price amount =
    let book =
      try Hashtbl.find books curr
      with Not_found -> Book.empty in
    let new_book = Book.update book price amount in
    Hashtbl.replace books curr new_book

  let print books =
    let print_one book = IntMap.iter
      (fun rate amount -> Printf.printf "(%f,%f) "
        (Satoshi.to_btc_float rate)
        (Satoshi.to_btc_float amount)) book in
    Hashtbl.iter (fun curr book ->
      Printf.printf "Currency: %s\n" (Currency.to_string curr);
      print_one book; print_endline "";
    ) books
end

module type BOOK = sig
  val update_books : Order.kind -> Currency.t -> int -> int -> unit
  val add_books : Order.kind -> Currency.t -> int -> int -> unit
end

module MyBooks = struct
  let bid_books = Books.empty ()
  let ask_books = Books.empty ()

  let update_books = function
    | Order.Bid -> Books.update bid_books
    | Order.Ask -> Books.update ask_books

  let add_books = function
    | Order.Bid -> Books.add bid_books
    | Order.Ask -> Books.add ask_books

end
