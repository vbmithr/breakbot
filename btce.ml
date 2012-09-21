module YS = Yojson.Safe
open Cohttp_lwt_unix

open Utils
open Common

let period = 2.0
let exchanges = ["btce", "https://btc-e.com/api/2/btc_usd/depth";
                 "campbx", "http://campbx.com/api/xdepth.php";
                 "bitstamp", "https://www.bitstamp.net/api/order_book"
                ]
let exchanges = List.map (fun (a,b) -> a, Uri.of_string b) exchanges
let buf = Bi_outbuf.create 4096

module Make = functor (B : BOOK) -> struct
  let parse ?buf body_str =
    let parse_btce_array kind = function
      | `List json_list ->
        List.iter (function
        | `List [price; amount] ->
          let price, amount =
            match price, amount with
              | `Int price, `Int amount     ->
                float_of_int price, float_of_int amount
              | `Int price, `Float amount   -> float_of_int price, amount
              | `Float price, `Int amount   -> price, float_of_int amount
              | `Float price, `Float amount -> price, amount
              | `String price, `String amount ->
                float_of_string price, float_of_string amount
              | _ -> failwith "parse_btce_array: Invalid input 2" in
          B.add_books kind Currency.USD
            (Satoshi.of_btc_float price) (Satoshi.of_btc_float amount)
        | _ -> failwith "parse_btce_array: Invalid input 1"
        ) json_list
      | _ -> failwith "parse_btce_array: Invalid input 0"
    in
    let body_json =
      match buf with
        | None -> YS.from_string body_str
        | Some buf -> YS.from_string ~buf body_str in

    match body_json with
      | `Assoc [(kind1, value1); (kind2, value2)] ->
        let asks, bids =
          match (Order.kind_of_string kind1), (Order.kind_of_string kind2) with
            | Order.Ask, Order.Bid -> value1, value2
            | Order.Bid, Order.Ask -> value2, value1
            | _                    -> failwith "Parsing: Import format error"
        in
        B.clear_books ();
        parse_btce_array Order.Ask asks;
        parse_btce_array Order.Bid bids

      | _ -> failwith "Parser.parse"
end

module Parser = Make(MyBooks)

let rec update_depth () =
  lwt res = Client.get (List.assoc Sys.argv.(1) exchanges) in
  match res with
    | None -> let () = Printf.printf "No response!\n%!" in update_depth ()
    | Some (response, body) ->
      lwt body_str = Body.string_of_body body in
      let () = Parser.parse ~buf body_str in
      lwt () = Lwt_unix.sleep period in update_depth ()

(* Entry point *)
let () =
  Sys.catch_break true;
  try
    Lwt_main.run (update_depth ())
  with Sys.Break ->
    print_endline "Bids"; Books.print MyBooks.bid_books;
    print_endline "Asks"; Books.print MyBooks.ask_books
