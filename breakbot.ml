open Utils
open Common
open Exchange

(* Works only if processing is much faster than the rate at which we
   receive/parse the messages from the exchange. Otherwise processing
   might block the exchanges from updating... It has thus to be the
   case that processing is indeed faster than receiving+parsing *)

let () =
  let config = Config.of_file "breakbot.conf" in
  let mtgox_key, mtgox_secret = match (List.assoc "mtgox" config) with
    | [key; secret] ->
      Uuidm.to_bytes (Opt.unopt (Uuidm.of_string key)),
      Cohttp.Base64.decode secret
    | _ -> failwith "Syntax error in config file."
  and intersango_key = match (List.assoc "intersango" config) with
    | [key] -> key
    | _ -> failwith "Syntax error in config file."
  in
  let exchanges =
    [(new Intersango.intersango intersango_key :> Exchange.exchange);
     (new Mtgox.mtgox mtgox_key mtgox_secret :> Exchange.exchange)] in
  let mvars = List.map (fun xch -> xch#get_mvar) exchanges in
  let process mvars =
    lwt converters = Ecb.converters in
    let rec process () =
      lwt xch = Lwt.pick (List.map Lwt_mvar.take mvars) in
      let () = Printf.printf "Exchange %s has just been updated!\n" xch#name in
      let other_xchs = List.filter (fun x -> x != xch) exchanges in
      let arbiter_one x1 x2 =
        let () = Printf.printf "Arbitrage table for: %s <-> %s\n%!"
          x1#name x2#name in
        let common_currs = StringSet.inter x1#currs x2#currs in
        let res =
          StringSet.fold
            (fun curr acc ->
              let ret =
                try
                  Books.arbiter_unsafe
                    curr converters x1#get_books x1#base_curr
                    x2#get_books x2#base_curr
                with Not_found -> Z.((~$0,~$0),(~$0,~$0))
              in ((curr, ret)::acc)
            ) common_currs [] in
        List.iter (fun (curr, ((qty1,pr1), (qty2,pr2))) ->
          Printf.printf "%s -> : %f (%f %s)\n%!"
            curr (Satoshi.to_face_float qty1) Z.(to_float pr1 /. 1e13) curr;
          Printf.printf "%s <- : %f (%f %s)\n%!"
            curr (Satoshi.to_face_float qty2) Z.(to_float pr2 /. 1e13) curr)
          res in
      let () = List.iter (fun x -> arbiter_one xch x) other_xchs in
      process ()
    in process ()
  in
  let threads_to_run =
    process mvars :: List.map (fun xch -> xch#update) exchanges in
  Lwt.pick threads_to_run |> Lwt_main.run
