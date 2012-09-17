open Yojson.Safe

open Common
open Intersango_common

module type BOOK = sig
  val add_to_bid_books : Currency.t -> int -> int -> unit
  val add_to_ask_books : Currency.t -> int -> int -> unit
end

module Parser = functor (B : BOOK) -> struct
  let parse_orderbook3 curr kind = function
    | `Assoc l ->
      List.iter (fun (rate, amount) ->
        match amount with `String amount ->
          (let rate = Satoshi.of_btc_string rate in
           let amount = Satoshi.of_btc_string amount in
           match kind with
             | Order.Bid -> B.add_to_bid_books curr rate amount
             | Order.Ask -> B.add_to_ask_books curr rate amount)
          | _ -> failwith "Parser.parse_orderbook3"
      ) l
    | _        -> failwith "Parser.parse_orderbook3"

  let parse_orderbook2 curr = function
    | `Assoc l ->
      List.iter (fun (kind, obj) ->
        match kind with
          | "bids" -> parse_orderbook3 curr Order.Bid obj
          | "asks" -> parse_orderbook3 curr Order.Ask obj
          | _      -> failwith "Parser.parse_orderbook2") l
    | _        -> failwith "Parser.parse_orderbook2"

  let parse_orderbook1 = function
    | `Assoc l ->
      List.iter (fun (curr, obj) ->
        let curr = Currency.of_int (int_of_string curr) in
        parse_orderbook2 curr obj) l
    | _        -> failwith "Parser.parse_orderbook1"

  let update_books = function
    | `List [`String "orderbook"; obj] ->
      parse_orderbook1 obj

    | `List [`String "depth";
       `Assoc [
         "currency_pair_id", `String curr;
         "rate", `String rate;
         "type", `String kind;
         "amount", `String amount
       ]]    ->
      let curr = Currency.of_int (int_of_string curr) in
      let rate = Satoshi.of_btc_string rate in
      let amount = Satoshi.of_btc_string amount in
      (match kind with
        | "bids" -> B.add_to_bid_books curr rate amount
        | "asks" -> B.add_to_ask_books curr rate amount
        | _      -> failwith "Parser.update_books")
    | `List [`String "ping"; obj] -> assert (obj = `Assoc [])
    | _ -> () (* Do nothing on other messages *)
end
