open Utils
open Common
open Cmdliner

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

let main () =
  lwt () = Lwt_unix.sleep 0.5 in
  lwt b_mtgox = mtgox#get_balances
  and b_intersango = intersango#get_balances in
  (* lwt _ = mtgox#withdraw_btc Z.(~$1000000) "1FTyBHz1C3nYYkRPhGbdRck4VYaRWkbDM3" in *)
  (* lwt _ = mtgox#place_order Order.Ask "USD" S.(~$10000000000) S.(~$100000000) in *)
  print_balances "MtGox" b_mtgox;
  print_balances "Intersango" b_intersango;
  Lwt.return ()

let () =
  Lwt_main.run $ main ()
