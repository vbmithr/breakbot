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

(* Works only if processing is much faster than the rate at which we
   receive/parse the messages from the exchange. Otherwise processing
   might block the exchanges from updating... It has thus to be the
   case that processing is indeed faster than receiving+parsing *)

let nb_of_iter = 100
let min_ratio = 0.0

let main () =
  let template = "$(date).$(milliseconds) [$(level)]: $(message)" in
  let std_logger =
    Lwt_log.channel ~template ~close_mode:`Keep ~channel:Lwt_io.stdout () in
  lwt file_logger = Lwt_log.file ~template ~file_name:"breakbot.log" () in
  let brd_logger = Lwt_log.broadcast [std_logger; file_logger] in
  let () = Lwt_log.default := brd_logger in
  let config = Config.of_file "breakbot.conf" in
  let mtgox_key, mtgox_secret, mtgox_addr  =
    match (List.assoc "mtgox" config) with
    | [key; secret; addr] ->
      Uuidm.to_bytes (Opt.unbox (Uuidm.of_string key)),
      Cohttp.Base64.decode secret, addr
    | _ -> failwith "Syntax error in config file."
  and btce_key, btce_secret, btce_addr =
    match (List.assoc "btce" config) with
    | [key; secret; addr] -> key, secret, addr
    | _ -> failwith "Syntax error in config file."
  and bs_login, bs_passwd, bs_addr =
    match (List.assoc "bitstamp" config) with
    | [login; passwd; addr] -> login, passwd, addr
    | _ -> failwith "Syntax error in config file."
  in
  let ustream, push_f = Lwt_stream.create () in
  let exchanges_assq =
    ["mtgox", (new Mtgox.mtgox mtgox_key mtgox_secret mtgox_addr push_f
               :> Exchange.exchange);
     "btce", (new Btce.btce btce_key btce_secret btce_addr push_f
              :> Exchange.exchange);
     "bitstamp", (new Bitstamp.bitstamp bs_login bs_passwd bs_addr push_f
                  :> Exchange.exchange)
    ] in
  let exchanges = List.map snd exchanges_assq in
  let rec process ustream =
    lwt updated_xch = Lwt_stream.get ustream >|= Opt.unbox in
    let updated_xchs = List.assoc updated_xch exchanges_assq in
    let arbiter_all xch =
      let other_xchs = List.filter (fun x -> x != xch) exchanges in
      let arbiter_one x1 x2 =
        try_lwt
          let direction, (_, qty, spr, bpr) = Books.arbiter_unsafe
              "USD" x1#get_books x2#get_books nb_of_iter min_ratio in
          let real_gain, ratio =
            let bfees, sfees = if direction then x1#fee, x2#fee
              else x2#fee, x1#fee in
            let spr_float, bpr_float = S.to_float spr, S.to_float bpr in
            let gain_float = (spr_float *. sfees -. bpr_float *. bfees) in
            gain_float /. 1e16, gain_float /. bpr_float in
          Lwt_log.notice_f "%s\t %s \t%s: %f (%f USD, %f %%)\n%!"
            x1#name
            ((function true -> "->"| false -> "<-") direction)
            x2#name
            (S.to_face_float qty) real_gain (ratio *. 100.0)
        with Not_found -> Lwt.return () in
      Lwt_list.iter_s (fun x -> arbiter_one xch x) other_xchs in
    arbiter_all updated_xchs >> process ustream
  in
  Lwt.join @@ process ustream :: List.map (fun xch -> xch#update) exchanges

let () = Lwt_main.run @@ main ()
