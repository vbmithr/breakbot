(*
 * Copyright (c) 2012 Vincent Bernardoff <vb@luminar.eu.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Utils
open Lwt_utils
open Common

let config = Config.of_file "breakbot.conf"
let mtgox_key, mtgox_secret, mtgox_addr =
  match (List.assoc "mtgox" config) with
    | [key; secret; addr] ->
      Uuidm.to_bytes (Opt.unbox (Uuidm.of_string key)),
      Cohttp.Base64.decode secret, addr
    | _ -> failwith "Syntax error in config file."
and btce_key, btce_secret, btce_addr =
  match (List.assoc "btce" config) with
    | [key; secret; addr] -> key, secret, addr
    | _ -> failwith "Syntax error in config file."
and bstamp_l, bstamp_p, bstamp_addr =
  match (List.assoc "bitstamp" config) with
    | [login; passwd; addr] -> login, passwd, addr
    | _ -> failwith "Syntax error in config file."

let exchanges = [
  (new Mtgox.mtgox mtgox_key mtgox_secret mtgox_addr ignore :> Exchange.exchange);
  (new Btce.btce btce_key btce_secret btce_addr ignore :> Exchange.exchange);
  (new Bitstamp.bitstamp bstamp_l bstamp_p bstamp_addr ignore :> Exchange.exchange)
]

let print_balances ?curr xchs =
  lwt balances = Lwt_list.map_p
    (fun x -> x#get_balances >|= fun b -> x#name, b) xchs in
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
        (fun x -> x#get_tickers >|= fun t -> x#name, t) xchs in
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
    | Some name -> List.filter (fun x -> x#name = name) exchanges
    | None      -> exchanges in
  if xchs = []
  then `Error (false,
               Printf.sprintf "Exchange %s unknown" (Opt.unbox xch))
  else `Ok (f ?curr xchs)

let place_order xch kind curr price amount =
  try_lwt
    let xch = List.find (fun x -> x#name = xch) exchanges in
    lwt rpc =
      xch#place_order kind curr
        (S.of_face_float price) (S.of_face_float amount)
    in Lwt_io.printl $ Jsonrpc.to_string rpc
  with
    | Not_found -> Lwt_io.eprintf "Exchange %s unknown\n" xch
    | Failure msg -> Lwt_io.eprintl msg

let withdraw_btc xch amount address =
  try_lwt
    let xch = List.find (fun x -> x#name = xch) exchanges in
    let address =
      if address.[0] = '1' then address else
        let target_xch =
          try List.find (fun x -> x#name = address) exchanges
          with Not_found ->
            failwith (Printf.sprintf "Exchange %s unknown" address) in
        target_xch#get_btc_addr in
    lwt rpc =
      xch#withdraw_btc (S.of_face_float amount) address
    in Lwt_io.printl $ Jsonrpc.to_string rpc
  with
    | Not_found -> Lwt_io.eprintf "Exchange %s unknown\n" xch
    | Failure msg -> Lwt_io.eprintl msg

open Cmdliner

(* Arguments *)

let currency_arg =
  let doc = "Currency to use, defaults to USD." in
  Arg.(value & opt string "USD" & info ~doc ~docv:"CURRENCY"
         ["c";"curr";"currency"])

(* Commands *)

let buy_cmd =
  let exchange_arg =
    let doc = "Exchange to use." in
    Arg.(required & pos 0 (some string) None & info ~doc [] ~docv:"EXCHANGE")
  and amount_arg =
    let doc = "Amount of BTC." in
    Arg.(required & pos 1 (some float) None & info ~doc [] ~docv:"AMOUNT")
  and price_arg =
    let doc = "Price for 1BTC in currency." in
    Arg.(required & pos 2 (some float) None & info ~doc [] ~docv:"PRICE")
  and doc = "Buy bitcoins." in
  Term.(pure place_order $ exchange_arg $ pure Order.Bid
          $ currency_arg $ price_arg $ amount_arg),
  Term.info "buy" ~doc

let sell_cmd =
  let exchange_arg =
    let doc = "Exchange to use." in
    Arg.(required & pos 0 (some string) None & info ~doc [] ~docv:"EXCHANGE")
  and amount_arg =
    let doc = "Amount of BTC." in
    Arg.(required & pos 1 (some float) None & info ~doc [] ~docv:"AMOUNT")
  and price_arg =
    let doc = "Price for 1BTC in currency." in
    Arg.(required & pos 2 (some float) None & info ~doc [] ~docv:"PRICE")
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
  let exchange_arg =
    let doc = "Exchange to use." in
    Arg.(required & pos 0 (some string) None & info ~doc [] ~docv:"EXCHANGE")
  and amount_arg =
    let doc = "Amount of BTC." in
    Arg.(required & pos 1 (some float) None & info ~doc [] ~docv:"AMOUNT")
  and addr_arg =
    let doc = "BTC destination address." in
    Arg.(required & pos 2 (some string) None & info ~doc [] ~docv:"PRICE")
  and doc = "Withdraw bitcoins" in
  Term.(pure withdraw_btc $ exchange_arg $ amount_arg $ addr_arg),
  Term.info "withdraw" ~doc

let move_cmd =
  let doc = "Perform arbitrage between two exchanges." in
  Term.(pure (Lwt.return ())),
  Term.info "move" ~doc

let exchange_cmd =
  let doc = "Display available exchanges." in
  Term.(pure (fun () -> Lwt_list.iter_s
                (fun x -> Lwt_io.printf "%s\t" x#name)
                exchanges >>= fun _ -> Lwt_io.print "\n") $ pure ()),
  Term.info "exchange" ~doc

let default_cmd =
  let doc = "A CLI to interact with bitcoin exchanges." in
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info "cli" ~version:"0.1" ~doc

let cmds = [exchange_cmd; buy_cmd; sell_cmd; ticker_cmd;
            balance_cmd; withdraw_cmd; move_cmd]

let main = match Term.eval_choice default_cmd cmds with
  | `Ok x -> x | _ -> exit 1

let () = Lwt_main.run main
