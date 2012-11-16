open Utils
open Common
open Cmdliner

module AT = ANSITerminal

let config = Config.of_file "breakbot.conf"
let mtgox_key, mtgox_secret = match (List.assoc "mtgox" config) with
  | [key; secret] ->
    Uuidm.to_bytes (Opt.unbox (Uuidm.of_string key)),
    Cohttp.Base64.decode secret
  | _ -> failwith "Syntax error in config file."
and intersango_key = match (List.assoc "intersango" config) with
  | [key] -> key
  | _ -> failwith "Syntax error in config file."

let mtgox      = new Mtgox.mtgox mtgox_key mtgox_secret
let intersango = new Intersango.intersango intersango_key

let print_balances name pairs =
  Printf.printf "Balances for exchange %s\n" name;
  List.iter (fun (c, b) ->
    Printf.printf "%s: %f\n%!" c (S.to_face_float b)) pairs

let print_tickers name tickers =
  let open Ticker in
  Printf.printf "Tickers for exchange %s\n" name;
  List.iter (fun (c, t) ->
    Printf.printf "%s: %.2f %.2f %.2f\n%!"
      c
      (S.to_face_float t.bid)
      (S.to_face_float t.ask)
      (S.to_face_float t.last)
  ) tickers

let main () =
  lwt () = Lwt_unix.sleep 0.5 in
  lwt b_mtgox = mtgox#get_balances
  and b_intersango = intersango#get_balances
  and t_mtgox = mtgox#get_tickers
  and t_intersango = intersango#get_tickers in
  lwt rpc1 =
    try_lwt mtgox#withdraw_btc Z.(~$1000000) "1FTyBHz1C3nYYkRPhGbdRck4VYaRWkbDM3"
    with Failure msg -> Printf.printf "withdraw_btc error: %s\n" msg; Lwt.return Rpc.Null
  in
  lwt rpc2 =
    try_lwt intersango#withdraw_btc Z.(~$100000000) "1FTyBHz1C3nYYkRPhGbdRck4VYaRWkbDM3"
    with Failure msg -> Printf.printf "withdraw_btc error: %s\n" msg; Lwt.return Rpc.Null
  in
  lwt rpc3 =
    try_lwt intersango#place_order Order.Ask "EUR" S.(~$10000000000) S.(~$100000000)
    with Failure msg -> Printf.printf "place_order error: %s\n" msg; Lwt.return Rpc.Null
  in
  lwt rpc4 =
    try_lwt mtgox#place_order Order.Ask "EUR" S.(~$10000000000) S.(~$100000000)
    with Failure msg -> Printf.printf "place_order error: %s\n" msg; Lwt.return Rpc.Null
  in
  Printf.printf "%s\n" (Jsonrpc.to_string rpc4);
  print_balances "MtGox" b_mtgox;
  print_balances "Intersango" b_intersango;
  print_tickers "MtGox" t_mtgox;
  print_tickers "Intersango" t_intersango;
  Lwt.return ()

let () =
  Lwt_main.run $ main ()
