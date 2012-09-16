let (|>) f g = g f
let (>>=) = Lwt.bind

module Opt = struct
  exception Unopt_none

  let unopt ?default v =
    match default, v with
      | _, Some v    -> v
      | Some d, None -> d
      | _            -> raise Unopt_none
end

open Lwt_unix

let tcp_conn_flags = [AI_FAMILY(PF_INET);
                      (* AI_FAMILY(PF_INET6);  *)
                      AI_SOCKTYPE(SOCK_STREAM)]

exception Resolv_error

let with_connection node service f =
  lwt addr_infos = getaddrinfo node service tcp_conn_flags in
  let addr_info =
    match addr_infos with h::t -> h | [] -> raise Resolv_error in
  Lwt_io.with_connection addr_info.ai_addr f

let open_connection node service =
  lwt addr_infos = getaddrinfo node service tcp_conn_flags in
  let addr_info =
    match addr_infos with h::t -> h | [] -> raise Resolv_error in
  Lwt_io.open_connection addr_info.ai_addr

let print_to_stdout (ic, oc) : unit Lwt.t =
  let rec print_to_stdout () =
    Lwt.return ic
    >>= fun c -> Lwt_io.read_line c
    >>= fun str ->
    Lwt_io.printf "%s\n" str
    >>= fun () -> print_to_stdout ()
  in print_to_stdout ()
