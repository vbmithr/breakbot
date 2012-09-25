open Utils
open Common
open Exchange

open Intersango

(* Works only if processing is much faster than the rate at which we
   receive/parse the messages from the exchange. Otherwise processing
   might block the exchanges from updating... It has thus to be the
   case that processing is indeed faster than receiving+parsing *)

let () =
  Random.self_init ();
  Sys.catch_break true;
  (* Most performant method*)
  (* Lwt_unix.set_default_async_method Lwt_unix.Async_switch; *)
  let intersango = new Intersango.intersango in
  let mtgox = new Mtgox.mtgox in
  let mvars = [intersango#get_mvar; mtgox#get_mvar] in
  let rec process mvars =
    lwt xch = Lwt.pick (List.map Lwt_mvar.take mvars) in
    lwt () = Lwt_io.printf "Exchange %s has just been updated!\n" xch in
    process mvars in
  try
    let threads_to_run = [mtgox#update (); intersango#update (); process mvars] in
    Lwt.join threads_to_run |> Lwt_main.run
  with Sys.Break ->
    Printf.printf "Intersango:\n\n";
    intersango#print;
