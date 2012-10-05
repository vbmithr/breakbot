open Utils

module Currency = struct
  type t = GBP | EUR | USD | PLN | SEK

  let compare = Pervasives.compare

  let to_string = function
    | GBP -> "GBP"
    | EUR -> "EUR"
    | USD -> "USD"
    | PLN -> "PLN"
    | SEK -> "SEK"

  let of_string = function
    | "GBP" -> GBP
    | "EUR" -> EUR
    | "USD" -> USD
    | "PLN" -> PLN
    | "SEK" -> SEK
    | _ -> failwith "Currency.of_string"
end

module CurrencyMap = Map.Make(Currency)

module Cent = functor (R : (sig val value: float end)) -> struct
  type t = int64

  let of_face_float v = Int64.of_float (v *. R.value)
  let to_face_float v = Int64.to_float v /. R.value

  let of_string = Int64.of_string
  let of_face_string v = Int64.of_float (float_of_string v *. R.value)
end

module Satoshi = Cent (struct let value = 1e8 end)
module Dollar  = Cent (struct let value = 1e5 end)

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
        price     : int64; (* price in satoshis *)
        amount    : int64 (* amount of BTC in satoshis *)
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
  include Int64Map

  let update price amount book =
    try
      let old_amount = Int64Map.find price book in
      Int64Map.add price (old_amount +++ amount) book
    with Not_found -> Int64Map.add price amount book

  let amount_below_or_eq book price =
    let l, data, r = Int64Map.split price book in
    Int64Map.fold (fun pr am acc -> acc +++ am)
      l (Opt.unopt ~default:0L data)

  let amount_above_or_eq book price =
    let l, data, r = Int64Map.split price book in
    Int64Map.fold (fun pr am acc -> acc +++ am)
      r (Opt.unopt ~default:0L data)

  let diff book1 book2 =
    let merge_fun key v1 v2 = match v1, v2 with
      | None, None       -> failwith "Should never happen"
      | Some v1, None    -> Some (Int64.neg v1)
      | None, Some v2    -> Some v2
      | Some v1, Some v2 -> Some (v2 --- v1) in
    Int64Map.merge merge_fun book1 book2

  let patch book patch =
    let merge_fun key v1 v2 = match v1, v2 with
      | None, None -> failwith "Should never happen"
      | Some v1, None -> Some v1
      | None, Some v2 -> Some v2
      | Some v1, Some v2 -> Some (v1 +++ v2) in
    Int64Map.merge merge_fun book patch
end

module type BOOKS = sig
  type t
  val empty  : t

  val add    : t -> Currency.t -> Order.kind -> int64 -> int64 -> t
  val update : t -> Currency.t -> Order.kind -> int64 -> int64 -> t

  val remove : t -> Currency.t -> t

  val print  : t -> unit
end

(** Books are a hashtbl of BookPairs, one for each currency *)
module Books : BOOKS = struct
  type t = (int64 Book.t * int64 Book.t) CurrencyMap.t

  let empty = CurrencyMap.empty

  let modify action_fun books curr kind price amount =
    let bid, ask =
      try CurrencyMap.find curr books
      with Not_found -> (Book.empty, Book.empty)  in
    match kind with
      | Order.Bid ->
        let newbook = action_fun price amount bid in
        CurrencyMap.add curr (newbook, ask) books
      | Order.Ask ->
        let newbook = action_fun price amount ask in
        CurrencyMap.add curr (bid, newbook) books

  let add    = modify Book.add
  let update = modify Book.update

  let remove books curr = CurrencyMap.remove curr books

  let print books =
    let print_one book = Int64Map.iter
      (fun rate amount -> Printf.printf "(%f,%f) "
        (Satoshi.to_face_float rate)
        (Satoshi.to_face_float amount)) book in
    CurrencyMap.iter (fun curr (bid,_) ->
      Printf.printf "BID %s\n" (Currency.to_string curr);
      print_one bid; print_endline "";
    ) books;
    CurrencyMap.iter (fun curr (_,ask) ->
      Printf.printf "ASK %s\n" (Currency.to_string curr);
      print_one ask; print_endline "";
    ) books
end
