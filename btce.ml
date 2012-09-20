module YS = Yojson.Safe
open Cohttp_lwt_unix

open Utils
open Common

let period = 2.0
let url_string = "https://btc-e.com/api/2/btc_usd/depth"
(* let uri = Uri.of_string "https://btc-e.com/api/2/btc_usd/depth" *)
(* let uri = Uri.make ~scheme:"https" *)
(*   ~host:"btc-e.com" ~port:443 ~path:"/api/2/btc_usd/depth" () *)
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
      | `Assoc [("asks", asks); ("bids", bids)] ->
        parse_btce_array Order.Ask asks;
        parse_btce_array Order.Bid bids

      | _ -> failwith "Parser.parse"
end

module Parser = Make(MyBooks)

let rec update_depth () =
  (* lwt res = Client.get uri in *)
  (* match res with *)
  (*   | None -> let () = Printf.printf "No response!\n%!" in update_depth () *)
  (*   | Some (response, body) -> *)
  (*     lwt body_str = Body.string_of_body body in *)
  let body_str = Http_client.Convenience.http_get url_string in
  let () = Parser.parse ~buf body_str in
  lwt () = Lwt_unix.sleep period in update_depth ()

(* Entry point *)
let () =
  Sys.catch_break true;
  Ssl.init ();
  Http_client.Convenience.configure_pipeline
    (fun p ->
      let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
      let tct = Https_client.https_transport_channel_type ctx in
      p # configure_transport Http_client.https_cb_id tct
    );
  try
    Lwt_main.run (update_depth ())
  with Sys.Break ->
    print_endline "Bids"; Books.print MyBooks.bid_books;
    print_endline "Asks"; Books.print MyBooks.ask_books
