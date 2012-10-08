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
  include Int64

  let of_face_float v = of_float (v *. R.value)
  let to_face_float v = to_float v /. R.value
  let of_face_string v = of_float (float_of_string v *. R.value)
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

module type BOOK = sig
  include Map.S with type key = int64

  type value = int64 * int64

  val add : ?ts:int64 -> int64 -> int64 -> value t -> value t
  val update : ?ts:int64 -> int64 -> int64 -> value t -> value t

  val empty : value t

  val amount_below_or_eq : value t -> int64 -> int64
  val amount_above_or_eq : value t -> int64 -> int64
end

(** A book represent the depth for one currency, and one order kind *)
module Book : BOOK = struct
  include Int64Map

  type value = int64 * int64

  let add ?(ts=Unix.getmicrotime_int64 ()) price amount book =
    Int64Map.add price (amount, ts) book

  let update ?(ts=Unix.getmicrotime_int64 ()) price amount book =
    try
      let old_amount, _ = Int64Map.find price book in
      Int64Map.add price ((old_amount +++ amount), ts) book
    with Not_found -> Int64Map.add price (amount, ts) book

  let amount_below_or_eq book price =
    let l, data, r = Int64Map.split price book in
    Int64Map.fold (fun pr (am,_) acc -> acc +++ am)
      l (fst (Opt.unopt ~default:(0L,0L) data))

  let amount_above_or_eq book price =
    let l, data, r = Int64Map.split price book in
    Int64Map.fold (fun pr (am,_) acc -> acc +++ am)
      r (fst (Opt.unopt ~default:(0L, 0L) data))

  let diff book1 book2 =
    let merge_fun key v1 v2 = match v1, v2 with
      | None, None       -> failwith "Should never happen"
      | Some (v1,ts1) , None    -> Some ((Int64.neg v1), ts1)
      | None, Some (v2,ts2)    -> Some (v2, ts2)
      | Some (v1,ts1), Some (v2,ts2) -> Some ((v2 --- v1), (max ts1 ts2)) in
    Int64Map.merge merge_fun book1 book2

  let patch book patch =
    let merge_fun key v1 v2 = match v1, v2 with
      | None, None -> failwith "Should never happen"
      | Some (v1,ts1), None -> Some (v1, ts1)
      | None, Some (v2,ts2) -> Some (v2, ts2)
      | Some (v1,ts1), Some (v2,ts2) -> Some ((v1 +++ v2), (max ts1 ts2)) in
    Int64Map.merge merge_fun book patch
end


module BooksFunctor = struct
  module Make (B : BOOK) = struct
    type t = (B.value B.t * B.value B.t) CurrencyMap.t

    let empty = CurrencyMap.empty

    let modify
      (action_fun : ?ts:int64 -> int64 -> int64 -> B.value B.t -> B.value B.t)
      ?(ts=Unix.getmicrotime_int64 ())
      books curr kind price amount =
      let bid, ask =
        try CurrencyMap.find curr books
        with Not_found -> (B.empty, B.empty)  in
      match kind with
        | Order.Bid ->
          let newbook = action_fun ~ts price amount bid in
          CurrencyMap.add curr (newbook, ask) books
        | Order.Ask ->
          let newbook = action_fun ~ts price amount ask in
          CurrencyMap.add curr (bid, newbook) books

    let add = modify B.add
    let update = modify B.update

    let remove books curr = CurrencyMap.remove curr books

    let print books =
      let print_one book = Book.iter
        (fun price (amount,ts) -> Printf.printf "(%f,%f) "
          (Dollar.to_face_float price)
          (Satoshi.to_face_float amount)) book in
      CurrencyMap.iter (fun curr (bid,ask) ->
        Printf.printf "BID %s\n" (Currency.to_string curr);
        print_one bid; print_endline "";
        Printf.printf "ASK %s\n" (Currency.to_string curr);
        print_one ask; print_endline ""
      ) books
  end
end

module Books = BooksFunctor.Make(Book)
