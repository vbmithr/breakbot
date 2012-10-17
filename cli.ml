open Utils
open Common
open Cmdliner

let config = Config.of_file "breakbot.conf"
let mtgox_key, mtgox_secret = match (List.assoc "mtgox" config) with
  | [key; secret] ->
    Uuidm.to_bytes (Opt.unopt (Uuidm.of_string key)),
    Cohttp.Base64.decode secret
  | _ -> failwith "Syntax error in config file."
and intersango_key = match (List.assoc "intersango" config) with
  | [key] -> key
  | _ -> failwith "Syntax error in config file."

let mtgox = new Mtgox.mtgox mtgox_key mtgox_secret
let intersango = new Intersango.intersango intersango_key

let print_balances =
  lwt () = Lwt_unix.sleep 3.0 in
  lwt balances = intersango#get_balances in
  let () = List.iter (fun (c, b) ->
    Printf.printf "%s: %f\n%!" c (Satoshi.to_face_float b)) balances
  in Lwt.return ()

let () =
  Lwt_main.run print_balances
