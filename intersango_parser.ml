open Yojson.Safe

open Common
open Intersango_common

module Parser = struct
  let parse books json =
    let parse_orderbook3 curr kind = function
      | `Assoc l ->
        List.iter (fun (rate, amount) ->
          match amount with `String amount ->
            (let rate = Satoshi.of_btc_string rate in
             let amount = Satoshi.of_btc_string amount in
           Books.update books curr kind rate amount)
            | _ -> failwith "Parser.parse_orderbook3"
        ) l
      | _        -> failwith "Parser.parse_orderbook3" in
    let parse_orderbook2 curr = function
      | `Assoc l ->
        List.iter (fun (kind, obj) ->
          let kind = Order.kind_of_string kind in
          parse_orderbook3 curr kind obj) l
      | _        -> failwith "Parser.parse_orderbook2" in
    let parse_orderbook1 = function
      | `Assoc l ->
        List.iter (fun (curr, obj) ->
          let curr = Currency.of_int (int_of_string curr) in
          parse_orderbook2 curr obj) l
      | _        -> failwith "Parser.parse_orderbook1" in
    match json with
      | `List [`String "orderbook"; obj] ->
        parse_orderbook1 obj

      | `List [`String "depth";
               `Assoc [
                 "currency_pair_id", `String curr;
                 "rate", `String rate;
                 "type", `String kind;
                 "amount", `String amount
               ]]    ->
        let kind = Order.kind_of_string kind in
        let curr = Currency.of_int (int_of_string curr) in
        let rate = Satoshi.of_btc_string rate in
        let amount = Satoshi.of_btc_string amount in
        Books.update books curr kind rate amount
      | `List [`String "ping"; obj] -> assert (obj = `Assoc [])
      | _ -> () (* Do nothing on other messages *)
end
