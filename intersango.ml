open Utils
open Common
open Intersango_parser

let books = Books.empty ()

let print_to_stdout (ic, oc) : unit Lwt.t =
  let buf = Bi_outbuf.create 4096 in
  let rec print_to_stdout () =
    lwt line = Lwt_io.read_line ic in
    let () = Parser.parse books (Yojson.Safe.from_string ~buf line) in
    lwt () = Lwt_io.printf "%s\n" line in
    print_to_stdout ()
  in print_to_stdout ()

let () =
  Sys.catch_break true;
  try
    let threads_to_run =
      [(with_connection "db.intersango.com" "1337" print_to_stdout)] in
    Lwt.join threads_to_run |> Lwt_main.run
  with Sys.Break ->
    Books.print books
