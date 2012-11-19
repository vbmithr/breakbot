open Utils

module Cent = functor (R : (sig val value: float end)) -> struct
  include Z

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

  type value = S.t * int64

  val add : ?ts:int64 -> S.t -> S.t -> value t -> value t
  val update : ?ts:int64 -> S.t -> S.t -> value t -> value t

  val sum : ?min_v:S.t -> ?max_v:S.t -> value t -> S.t
  val buy_price : value t -> S.t -> S.t
  val sell_price : value t -> S.t -> S.t

  val amount_below_or_eq : value t -> S.t -> S.t
  val amount_above_or_eq : value t -> S.t -> S.t

  val arbiter_unsafe : value t -> value t -> S.t * S.t * S.t
end

(** A book represent the depth for one currency, and one order kind *)
module Book : BOOK = struct
  include SMap

  type value = S.t * int64

  let of_bindings bds =
    List.fold_left (fun acc (k,v) -> SMap.add k v acc) SMap.empty bds

  let min_value book =
    let open S in
        SMap.fold (fun _ v acc -> min v acc) book S.(pow ~$2 128)

  let add ?(ts=Unix.getmicrotime_int64 ()) price amount book =
    SMap.add price (amount,ts) book

  let update ?(ts=Unix.getmicrotime_int64 ()) price amount book =
    try
      let old_amount, _ = SMap.find price book in
      SMap.add price (S.(old_amount + amount), ts) book
    with Not_found -> SMap.add price (amount, ts) book

  let sum ?(min_v=S.(-pow ~$2 128)) ?(max_v=S.(pow ~$2 128)) book =
    let open S in
        SMap.fold
          (fun pr (am,_) acc ->
            if (geq pr min_v) && (leq pr max_v)
            then acc + pr*am else acc)
          book ~$0

  let to_depth book = function
    | Order.Bid ->
      let open S in
          let bindings = SMap.bindings book in
          fst $ List.fold_right (fun (pr,(am,_)) (depth, prev_sum) ->
            SMap.add pr (am + prev_sum) depth, am + prev_sum)
            bindings (SMap.empty, ~$0)

    | Order.Ask ->
      let open S in
          let _, new_book =
            SMap.fold (fun pr (am,_) (prev_sum, acc) ->
              (prev_sum + am), SMap.add pr (prev_sum + am) acc
            ) book (~$0,SMap.empty) in
          new_book

  let merge_max = SMap.merge (fun _ v1 v2 -> match v1, v2 with
    | Some v1, Some v2 -> Some S.(max v1 v2)
    | Some v1, None    -> Some v1
    | None,    Some v2 -> Some v2
    | None,    None    -> None)

  let buy_price ask_book amount =
    let open S in
        fst (SMap.fold (fun pr (am,_) (pr_, am_) ->
          match sign (am_ - am) with
            | 1 -> (pr_ + pr*am, am_ - am)
            | 0 -> (pr_ + pr*am, am_ - am)
            | -1 -> if gt am_ ~$0 then (pr_ + pr*am_, ~$0) else (pr_,am_)
            | _ -> failwith ""
        ) ask_book (~$0, amount))

  let sell_price bid_book amount =
    let open S in
        let bindings = SMap.bindings bid_book in
        fst (List.fold_right (fun (pr, (am,_)) (pr_,am_) ->
          match sign (am_ - am) with
            | 1 -> (pr_ + pr*am, am_ - am)
            | 0 -> (pr_ + pr*am, am_ - am)
            | -1 -> if gt am_ ~$0 then (pr_ + pr*am_, ~$0) else (pr_,am_)
            | _ -> failwith ""
        ) bindings (~$0, amount))

  let amount_below_or_eq book price =
    let open S in
        let l, data, r = SMap.split price book in
        (SMap.fold (fun pr (am,_) acc -> acc + am)
           l (fst (Opt.default (~$0,0L) data)))

  let amount_above_or_eq book price =
    let open S in
        let l, data, r = SMap.split price book in
        (SMap.fold (fun pr (am,_) acc -> acc + am)
           r (fst (Opt.default (~$0,0L) data)))

  let keys_between_or_eq book a b =
    let open S in
        SMap.fold (fun pr _ acc ->
          if geq pr a && leq pr b then pr::acc else acc)
          book []

  (** Gives the (quantity, amount_to_gain) that can be
      arbitraged. This function does not check that [bid] and [ask]
      are really bid resp. ask books *)
  let arbiter_unsafe bid ask =
    let open S in
        let max_bid = fst (max_binding bid)
        and min_ask = fst (min_binding ask) in
        if gt max_bid min_ask then
          let bid_keys = keys_between_or_eq bid min_ask max_bid
          and ask_keys = keys_between_or_eq ask min_ask max_bid in
          List.fold_left (fun acc pr ->
            let amount_below = amount_below_or_eq ask pr
            and amount_above = amount_above_or_eq bid pr in
            let min_qty = min amount_below amount_above in
            let buy_pr = buy_price ask min_qty
            and sell_pr = sell_price bid min_qty in
            Pervasives.max (sell_pr - buy_pr, pr, min_qty) acc
          ) (~$0, ~$0, ~$0) $ bid_keys @ ask_keys
        else
          S.(~$0, ~$0, ~$0)

  let diff book1 book2 =
    let open S in
        let merge_fun key v1 v2 = match v1, v2 with
          | None, None       -> failwith "Should never happen"
          | Some (v1,ts1) , None    -> Some ((~- v1), ts1)
          | None, Some (v2,ts2)    -> Some (v2, ts2)
          | Some (v1,ts1), Some (v2,ts2) -> Some ((v2 - v1), (max ts1 ts2)) in
        SMap.merge merge_fun book1 book2

  let patch book patch =
    let open S in
        let merge_fun key v1 v2 = match v1, v2 with
          | None, None -> failwith "Should never happen"
          | Some (v1,ts1), None -> Some (v1, ts1)
          | None, Some (v2,ts2) -> Some (v2, ts2)
          | Some (v1,ts1), Some (v2,ts2) -> Some ((v1 + v2), (max ts1 ts2)) in
        SMap.merge merge_fun book patch
end

module BooksFunctor = struct
  module Make (B : BOOK) = struct
    type t = (B.value B.t * B.value B.t) StringMap.t

    let (empty:t) = StringMap.empty

    let modify
        (action_fun :
           ?ts:int64 -> B.key -> B.key -> B.value B.t -> B.value B.t)
        ?(ts=Unix.getmicrotime_int64 ())
        books curr kind price amount =
      let bid, ask =
        try StringMap.find curr books
        with Not_found -> (B.empty, B.empty)  in
      match kind with
        | Order.Bid ->
          let newbook = action_fun ~ts price amount bid in
          StringMap.add curr (newbook, ask) books
        | Order.Ask ->
          let newbook = action_fun ~ts price amount ask in
          StringMap.add curr (bid, newbook) books

    let add = modify B.add
    let update = modify B.update

    let remove books curr = StringMap.remove curr books

    let arbiter_unsafe curr books1 books2 =
      let open S in
          let b1, a1 = StringMap.find curr books1
          and b2, a2 = StringMap.find curr books2 in
          let gain1, pr1, am1 = (B.arbiter_unsafe b2 a1)
          and gain2, pr2, am2 = (B.arbiter_unsafe b1 a2) in
          match sign $ gain1 - gain2 with
            | 1 -> 1, gain1, pr1, am1
            | 0 -> 0, ~$0, ~$0, ~$0
            | -1 -> -1, gain2, pr2, am2
            | _ -> failwith ""

    let print books =
      let print_one book = Book.iter
        (fun price (amount,ts) -> Printf.printf "(%f,%f) "
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

  class virtual exchange (name:string) =
  object (self)
    val mutable books = Books.empty
    val         mvar  = (Lwt_mvar.create_empty () : exchange Lwt_mvar.t)

    method name      = name
    method print     = Printf.printf "Books for exchange %s:\n%!" name;
      Books.print books
    method notify    = Lwt_mvar.put mvar (self :> exchange)
    method get_books = books
    method get_mvar  = mvar

    method virtual currs     : StringSet.t
    method virtual base_curr : string
    method virtual fees : int

    method virtual update    : unit Lwt.t
    method virtual place_order : Order.kind -> string -> S.t -> S.t ->
      Rpc.t Lwt.t
    method virtual withdraw_btc : S.t -> string -> Rpc.t Lwt.t
    method virtual get_balances : balances Lwt.t
  end
end
