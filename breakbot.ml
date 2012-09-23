open Utils
open Common
open Exchange

open Intersango

let () =
  Sys.catch_break true;
  (* Most performant method*)
  (* Lwt_unix.set_default_async_method Lwt_unix.Async_switch; *)
  let event, push = Lwt_react.E.create () in
  let intersango = new intersango push in
  let rec process event : unit Lwt.t =
    lwt updated_exchange = Lwt_react.E.next event in
    lwt () = Lwt_io.printf "Exchange %s has just been updated!\n%!"
      updated_exchange in
    process event in
  try
    let threads_to_run = [intersango#update (); process event] in
    Lwt.join threads_to_run |> Lwt_main.run
  with Sys.Break ->
    intersango#print
