open Utils

module Jsonrpc = struct
  include Jsonrpc

  let of_string_filter_null str =
    let rpc = of_string str in
    let open Rpc in
    let rec filter_null = function
      | Enum l -> Enum (List.map filter_null l)
      | Dict d -> Dict (
        List.map (fun (s,v) -> s, filter_null v) $
          List.filter (fun (s,v) -> v <> Null) d)
      | oth -> oth
    in filter_null rpc
end

module Cent = functor (R : (sig val value: float end)) -> struct
  include Z

  let of_face_float v = of_float (v *. R.value)
  let to_face_float v = to_float v /. R.value
  let of_face_string v = of_float (float_of_string v *. R.value)
  let to_face_string v = string_of_float (to_face_float v)
end

module S = Cent (struct let value = 1e8 end)

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

  val arbiter_unsafe : value t -> value t -> S.t * S.t
end

(** A book represent the depth for one currency, and one order kind *)
module Book : BOOK = struct
  include SMap

  type value = S.t * int64

  let of_bindings bds =
    List.fold_left (fun acc (k,v) -> SMap.add k v acc) SMap.empty bds

  let min_value book =
    let open Z in
        SMap.fold (fun _ v acc -> min v acc) book S.(pow ~$2 128)

  let add ?(ts=Unix.getmicrotime_int64 ()) (price:S.t) (amount:S.t) (book:value t) =
    SMap.add price (amount,ts) book

  let update ?(ts=Unix.getmicrotime_int64 ()) price amount book =
    try
      let old_amount, _ = SMap.find price book in
      SMap.add price (S.(old_amount + amount), ts) book
    with Not_found -> SMap.add price (amount, ts) book

  let sum ?(min_v=S.(-pow ~$2 128)) ?(max_v=S.(pow ~$2 128)) book =
    let open Z in
        SMap.fold
          (fun pr (am,_) acc ->
            if (geq pr min_v) && (leq pr max_v)
            then acc + pr*am else acc)
          book ~$0

  let to_depth book = function
    | Order.Bid ->
      let open Z in
          let bindings = SMap.bindings book in
          fst (List.fold_right (fun (pr,(am,_)) (depth, am_) ->
            SMap.add pr (am+am_) depth, am+am_) bindings (SMap.empty, ~$0))

    | Order.Ask ->
      let open Z in
          let _,bindings =
            SMap.fold (fun pr (am,_) (am_,bds) ->
              (am_ + am), (pr, am_+am)::bds
            ) book (~$0,[]) in
          of_bindings bindings

  let merge_max = SMap.merge (fun _ v1 v2 -> match v1, v2 with
    | Some v1, Some v2 -> Some S.(max v1 v2)
    | Some v1, None    -> Some v1
    | None,    Some v2 -> Some v2
    | None,    None    -> None)

  let buy_price ask_book amount =
    let open Z in
        fst (SMap.fold (fun pr (am,_) (pr_, am_) ->
          match sign (am_ - am) with
            | 1 -> (pr_ + pr*am, am_ - am)
            | 0 -> (pr_ + pr*am, am_ - am)
            | -1 -> if gt am_ ~$0 then (pr_ + pr*am_, ~$0) else (pr_,am_)
            | _ -> failwith ""
        ) ask_book (~$0, amount))

  let sell_price bid_book amount =
    let open Z in
        let bindings = SMap.bindings bid_book in
        fst (List.fold_right (fun (pr, (am,_)) (pr_,am_) ->
          match sign (am_ - am) with
            | 1 -> (pr_ + pr*am, am_ - am)
            | 0 -> (pr_ + pr*am, am_ - am)
            | -1 -> if gt am_ ~$0 then (pr_ + pr*am_, ~$0) else (pr_,am_)
            | _ -> failwith ""
        ) bindings (~$0, amount))

  let amount_below_or_eq book price =
    let open Z in
        let l, data, r = SMap.split price book in
        (SMap.fold (fun pr (am,_) acc -> acc + am)
           l (fst (Opt.unopt ~default:(~$0,0L) data)))

  let amount_above_or_eq book price =
    let open Z in
        let l, data, r = SMap.split price book in
        (SMap.fold (fun pr (am,_) acc -> acc + am)
           r (fst (Opt.unopt ~default:(~$0,0L) data)))

  (** Gives the quantity that can be arbitraged. This function does
      not check that [bid] and [ask] are really bid resp. ask books *)
  let arbiter_unsafe bid ask =
    let open Z in
        let max_bid = fst (max_binding bid)
        and min_ask = fst (min_binding ask) in
        if gt max_bid min_ask then
          let bid_depth = to_depth bid Order.Bid
          and ask_depth = to_depth ask Order.Ask in
          let merged = merge_max bid_depth ask_depth in
          let qty = min_value merged in qty, buy_price ask qty
        else
          S.(~$0, ~$0)

  let diff book1 book2 =
    let open Z in
        let merge_fun key v1 v2 = match v1, v2 with
          | None, None       -> failwith "Should never happen"
          | Some (v1,ts1) , None    -> Some ((~- v1), ts1)
          | None, Some (v2,ts2)    -> Some (v2, ts2)
          | Some (v1,ts1), Some (v2,ts2) -> Some ((v2 - v1), (max ts1 ts2)) in
        SMap.merge merge_fun book1 book2

  let patch book patch =
    let open Z in
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
        (action_fun : ?ts:int64 -> B.key -> B.key -> B.value B.t -> B.value B.t)
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

    let arbiter_unsafe curr convs (books1:t) basecurr1 (books2:t) basecurr2 =
      let convert_from_base (books:t) basecurr =
        let conv = convs basecurr curr in
        let b1, a1 = StringMap.find basecurr books in
        B.map (fun (am, ts) -> (conv am), ts) b1,
        B.map (fun (am, ts) -> (conv am), ts) a1 in

      let b1, a1 =
        try StringMap.find curr books1
        with Not_found -> convert_from_base books1 basecurr1
      and b2, a2 =
        try StringMap.find curr books2
        with Not_found -> convert_from_base books2 basecurr2
      in
      let qty1, pr1 = (B.arbiter_unsafe b2 a1)
      and qty2, pr2 = (B.arbiter_unsafe b1 a2) in
      match S.(sign (qty1 - qty2)) with
        | 1 ->
          let sell_pr = B.sell_price b2 qty1 in
          (qty1, S.(sell_pr - pr1)), (qty2, pr2)
        | 0 -> (qty1, pr1), (qty2, pr2)
        | -1 ->
          let sell_pr = B.sell_price b1 qty2 in
          (qty1, pr1), (qty2, S.(sell_pr - pr2))
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

  method virtual update    : unit Lwt.t
  method virtual place_order : Order.kind -> string -> S.t -> S.t ->
    Rpc.response Lwt.t
  method virtual withdraw_btc : S.t -> string -> Rpc.response Lwt.t
  method virtual get_balances : ((string * S.t) list) Lwt.t
end
