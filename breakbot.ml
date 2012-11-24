open Utils
open Lwt_utils
open Common

(* Works only if processing is much faster than the rate at which we
   receive/parse the messages from the exchange. Otherwise processing
   might block the exchanges from updating... It has thus to be the
   case that processing is indeed faster than receiving+parsing *)

let nb_of_iter = 100

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
  let exchanges =
    [(* (new Mtgox.mtgox mtgox_key mtgox_secret mtgox_addr :> Exchange.exchange); *)
     (new Btce.btce btce_key btce_secret btce_addr      :> Exchange.exchange);
     (new Bitstamp.bitstamp bs_login bs_passwd bs_addr  :> Exchange.exchange)
    ] in
  let mvars = List.map (fun xch -> xch#get_mvar) exchanges in
  let rec process mvars =
    lwt updated_xchs = Lwt.npick $ List.rev_map Lwt_mvar.take mvars in
    let arbiter_all xch =
      let other_xchs = List.filter (fun x -> x != xch) exchanges in
      let arbiter_one x1 x2 =
        try_lwt
          let (qty1, spr1, bpr1), (qty2, spr2, bpr2) = Books.arbiter_unsafe
            "USD" x1#get_books x2#get_books nb_of_iter in
          let real_gain (spr,sfees) (bpr,bfees) =
            ((S.to_float spr *. sfees -. S.to_float bpr *. bfees) /. 1e16) in
          Printf.printf "%s\t -> \t%s: %f (%f USD)\n%!"
            x1#name x2#name
            (S.to_face_float qty1) (real_gain (spr1,x2#fee) (bpr1,x1#fee));
          Printf.printf "%s\t <- \t%s: %f (%f USD)\n%!"
            x1#name x2#name
            (S.to_face_float qty2) (real_gain (spr2,x1#fee) (bpr2,x2#fee));
          Lwt.return ()
        with Not_found -> Lwt.return () in
      Lwt_list.iter_s (fun x -> arbiter_one xch x) other_xchs in
    Lwt_list.iter_s arbiter_all updated_xchs >> process mvars
  in
  Lwt.join $ process mvars :: List.map (fun xch -> xch#update) exchanges

let () = Lwt_main.run $ main ()
