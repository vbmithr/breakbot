open Utils
open Lwt_utils
open Common

let config = Config.of_file "breakbot.conf"
let mtgox_key, mtgox_secret = match (List.assoc "mtgox" config) with
  | [key; secret] ->
    Uuidm.to_bytes (Opt.unbox (Uuidm.of_string key)),
    Cohttp.Base64.decode secret
  | _ -> failwith "Syntax error in config file."
and btce_key, btce_secret = match (List.assoc "btce" config) with
  | [key; secret] -> key, secret
  | _ -> failwith "Syntax error in config file."
and bstamp_l, bstamp_p = match (List.assoc "bitstamp" config) with
  | [login; passwd] -> login, passwd
  | _ -> failwith "Syntax error in config file."

let exchanges = [
  "mtgox", (new Mtgox.mtgox mtgox_key mtgox_secret :> Exchange.exchange);
  "btce", (new Btce.btce btce_key btce_secret :> Exchange.exchange);
  "bitstamp", (new Bitstamp.bitstamp bstamp_l bstamp_p :> Exchange.exchange)
]

let print_balances ?curr xchs =
  lwt balances = Lwt_list.map_p
    (fun (n, x) -> x#get_balances >|= fun b -> (n, b)) xchs in
  Lwt_list.iter_s (fun (name, balances) ->
    Lwt_list.iter_s (fun (c, b) ->
      let str =
        Printf.sprintf "%s %s:\t%f\n%!" name c (S.to_face_float b) in
      match curr with
        | Some n -> if c = n then Lwt_io.print str else Lwt.return ()
        | None   -> Lwt_io.print str )
      balances) balances

let print_tickers ?curr xchs =
  let open Ticker in
      lwt tickers = Lwt_list.map_p
        (fun (n,x) -> x#get_tickers >|= fun t -> (n, t)) xchs in
      Lwt_list.iter_s (fun (name, tickers) ->
        Lwt_list.iter_s (fun (c, t) ->
          let str =
            Printf.sprintf "%s %s:\t%.2f %.2f %.2f\n%!"
              name c
              (S.to_face_float t.bid)
              (S.to_face_float t.ask)
              (S.to_face_float t.last) in
          match curr with
            | Some n -> if c = n then Lwt_io.print str else Lwt.return ()
            | None   -> Lwt_io.print str
        ) tickers) tickers

let run_print_fun xch curr f =
  let xchs = match xch with
    | Some name ->
      (try [List.find (fun (n, x) -> n = name) exchanges]
      with Not_found -> [])
    | None -> exchanges in
  if xchs = []
  then `Error (false,
               Printf.sprintf "Exchange \"%s\" unknown" (Opt.unbox xch))
  else `Ok (f ?curr xchs)

let place_order xch kind curr price amount =
  try_lwt
    let xch = List.assoc xch exchanges in
    lwt rpc =
      xch#place_order kind curr
        (S.of_face_float price) (S.of_face_float amount)
    in Lwt_io.printl $ Jsonrpc.to_string rpc
  with
    | Not_found -> Lwt_io.eprintf "Exchange \"%s\" unknown" xch
    | Failure msg -> Lwt_io.eprintl msg

open Cmdliner

(* Commands *)

let buy_cmd =
  let exchange_arg =
    let doc = "Exchange you want a ticker from, defaults to all." in
    Arg.(required & pos 0 (some string) None & info ~doc [] ~docv:"EXCHANGE")
  and amount_arg =
    let doc = "Amount of BTC." in
    Arg.(required & pos 1 (some float) None & info ~doc [] ~docv:"AMOUNT")
  and price_arg =
    let doc = "Price for 1BTC in currency." in
    Arg.(required & pos 2 (some float) None & info ~doc [] ~docv:"PRICE")
  and currency_arg =
    let doc = "Currency used, defaults to USD." in
    Arg.(value & pos 3 string "USD" & info ~doc [] ~docv:"CURRENCY")
  and doc = "Buy bitcoins." in
  Term.(pure place_order $ exchange_arg $ pure Order.Bid
          $ currency_arg $ price_arg $ amount_arg),
  Term.info "buy" ~doc

let sell_cmd =
  let exchange_arg =
    let doc = "Exchange you want a ticker from, defaults to all." in
    Arg.(required & pos 0 (some string) None & info ~doc [] ~docv:"EXCHANGE")
  and amount_arg =
    let doc = "Amount of BTC." in
    Arg.(required & pos 1 (some float) None & info ~doc [] ~docv:"AMOUNT")
  and price_arg =
    let doc = "Price for 1BTC in currency." in
    Arg.(required & pos 2 (some float) None & info ~doc [] ~docv:"PRICE")
  and currency_arg =
    let doc = "Currency used, defaults to USD." in
    Arg.(value & pos 3 string "USD" & info ~doc [] ~docv:"CURRENCY")
  and doc = "Sell bitcoins." in
  Term.(pure place_order $ exchange_arg $ pure Order.Ask
          $ currency_arg $ price_arg $ amount_arg),
  Term.info "sell" ~doc

let ticker_cmd =
  let exchange_arg =
    let doc = "Exchange you want tickers from, defaults to all." in
    Arg.(value & pos 0 (some string) None & info ~doc [] ~docv:"EXCHANGE")
  and currency_arg =
    let doc = "Currency that will be displayed, defaults to all." in
    Arg.(value & pos 1 (some string) None & info ~doc [] ~docv:"CURRENCY")
  and doc = "Display ticker for one or more exchange(s)." in
  Term.(ret (pure run_print_fun $ exchange_arg
               $ currency_arg $ pure print_tickers)),
  Term.info "ticker" ~doc

let balance_cmd =
  let exchange_arg =
    let doc = "Exchange you want balances from, defaults to all." in
    Arg.(value & pos 0 (some string) None & info ~doc [] ~docv:"EXCHANGE")
  and currency_arg =
    let doc = "Currency that will be displayed, defaults to all." in
    Arg.(value & pos 1 (some string) None & info ~doc [] ~docv:"CURRENCY")
  and doc = "Display balance for one or more exchange(s)." in
  Term.(ret (pure run_print_fun $ exchange_arg
               $ currency_arg $ pure print_balances)),
  Term.info "balance" ~doc

let withdraw_cmd =
  let doc = "Withdraw bitcoins" in
  Term.(pure (Lwt.return ())),
  Term.info "withdraw" ~doc

let move_cmd =
  let doc = "Perform arbitrage between two exchanges." in
  Term.(pure (Lwt.return ())),
  Term.info "move" ~doc

let default_cmd =
  let doc = "A CLI to interact with bitcoin exchanges." in
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info "cli" ~version:"0.1" ~doc

let cmds = [buy_cmd; sell_cmd; ticker_cmd;
            balance_cmd; withdraw_cmd; move_cmd]

let main = match Term.eval_choice default_cmd cmds with
  | `Ok x -> x | _ -> exit 1

let () = Lwt_main.run main
