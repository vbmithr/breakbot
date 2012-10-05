open Utils
open Common

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
  let parse_orderbook books decoder =
    let rec parse_orderbook ?curr ?kind ?price books =
      match Jsonm.decode decoder with
        | `Lexeme (`Name "bids") ->
          parse_orderbook ?price ?curr ~kind:Order.Bid books
        | `Lexeme (`Name "asks") ->
          parse_orderbook ?price ?curr ~kind:Order.Ask books
        | `Lexeme (`Name s) when String.is_int s ->
          let curr = Currency.of_int (int_of_string s)
          in parse_orderbook ?kind ?price ~curr books
        | `Lexeme (`Name s) when String.is_float s ->
          let price = Satoshi.of_face_string s in
          let amount = match Jsonm.decode decoder with
            | `Lexeme (`String am) -> Satoshi.of_face_string am
            | _ -> failwith "parse_orderbook" in
          let kind = Opt.unopt kind in
          let curr = Opt.unopt curr in
          let books = Books.update books curr kind price amount in
          parse_orderbook ~curr ~kind books
        | `Lexeme `Ae -> books
        | `Lexeme l -> parse_orderbook ?curr ?kind ?price books
        | `Error e -> Jsonm.pp_error Format.err_formatter e; books
        | `Await -> Printf.printf "Awaiting...\n%!"; books
        | `End -> Printf.printf "End...\n%!"; books
    in parse_orderbook books

  let parse_depth books decoder =
    let rec parse_depth ?name acc =
      match Jsonm.decode decoder with
        | `Lexeme (`Name s) -> parse_depth ~name:s acc
        | `Lexeme (`String s) -> let (name:string) = Opt.unopt name
                      in parse_depth ((name, s)::acc)
        | `Lexeme `Ae ->
          let kind = Order.kind_of_string (List.assoc "type" acc) in
          let curr = Currency.of_int
            (int_of_string ((List.assoc "currency_pair_id" acc))) in
          let price = Satoshi.of_face_string (List.assoc "rate" acc) in
          let amount = Satoshi.of_face_string (List.assoc "amount" acc) in
          Books.update books curr kind price amount
        | `Lexeme _ -> parse_depth acc
        | `Error e -> Jsonm.pp_error Format.err_formatter e; books
        | `Await -> Printf.printf "Awaiting...\n%!"; books
        | `End -> Printf.printf "End...\n%!"; books

    in parse_depth []

  let rec parse_jsonm books decoder =
    match Jsonm.decode decoder with
      | `Lexeme (`String "orderbook") -> parse_orderbook books decoder
      | `Lexeme (`String "depth")     -> parse_depth books decoder
      | `Lexeme l                     -> parse_jsonm books decoder
      | _  -> failwith "parse_jsonm"
end

class intersango =
object (self)
  inherit Exchange.exchange "intersango"

  method update =
    let rec update (ic, oc) =
      lwt line = Lwt_io.read_line ic in
      let () = Printf.printf "%s\n%!" line in
      let decoder = Jsonm.decoder (`String line) in
      let () = books <- Parser.parse_jsonm books decoder in
      lwt () = self#notify in
      update (ic, oc)
    in Lwt_io.with_connection_dns "db.intersango.com" "1337" update

  method bid curr price amount = Lwt.return ()
  method ask curr price amount = Lwt.return ()
end
