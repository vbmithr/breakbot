open Utils
open Common
open Exchange

open Intersango

(* Works only if processing is much faster than the rate at which we
   receive/parse the messages from the exchange. Otherwise processing
   might block the exchanges from updating... It has thus to be the
   case that processing is indeed faster than receiving+parsing *)

let () =
  Sys.catch_break true;
  (* Most performant method*)
  (* Lwt_unix.set_default_async_method Lwt_unix.Async_switch; *)
  let mvar = Lwt_mvar.create_empty () in
  let intersango = new intersango mvar in
  let rec process mvar =
    lwt (_:bool) = Lwt_mvar.take mvar in
    lwt () = Lwt_io.printf "Exchange has just been updated!\n" in
    process mvar in
  try
    let threads_to_run = [intersango#update (); process mvar] in
    Lwt.join threads_to_run |> Lwt_main.run
  with Sys.Break ->
    intersango#print
