open Utils
open Common

open Yojson.Safe

module Currency = struct
    include Common.Currency

  let to_int = function
    | GBP -> 1
    | EUR -> 2
    | USD -> 3
    | PLN -> 4
    | _ -> failwith "Currency.to_int"

  let of_int = function
    | 1 -> GBP
    | 2 -> EUR
    | 3 -> USD
    | 4 -> PLN
    | _ -> failwith "Currency.of_int"
end

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

class intersango (push : string -> unit) =
  let buf = Bi_outbuf.create 4096 in
object (self)
  inherit Exchange.exchange push
  method name = "intersango"

  method update () =
    let update (ic, oc) =
      lwt line = Lwt_io.read_line ic in
      let () = Parser.parse books (Yojson.Safe.from_string ~buf line) in
      let () = Printf.printf "I’m pushing the button.\n" in
      let () = self#push () in
      self#update ()
    in with_connection "db.intersango.com" "1337" update

  method bid curr price amount = Lwt.return ()
  method ask curr price amount = Lwt.return ()
end
