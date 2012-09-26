open Utils
open Common
open Exchange

(* Works only if processing is much faster than the rate at which we
   receive/parse the messages from the exchange. Otherwise processing
   might block the exchanges from updating... It has thus to be the
   case that processing is indeed faster than receiving+parsing *)

let () =
  Random.self_init ();
  Sys.catch_break true;
  (* Most performant method*)
  (* Lwt_unix.set_default_async_method Lwt_unix.Async_switch; *)
  let exchanges = [new Intersango.intersango;
                   new Mtgox.mtgox "key" "secret"] in
  let mvars = List.map (fun xch -> xch#get_mvar) exchanges in
  let rec process mvars =
    lwt xch = Lwt.pick (List.map Lwt_mvar.take mvars) in
    lwt () = Lwt_io.printf "Exchange %s has just been updated!\n" xch in
    process mvars in
  try
    let threads_to_run =
      process mvars :: List.map (fun xch -> xch#update) exchanges in
    Lwt.join threads_to_run |> Lwt_main.run
  with Sys.Break ->
    List.iter (fun xch -> xch#print) exchanges
