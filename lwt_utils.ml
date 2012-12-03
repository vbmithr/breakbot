let (>>=)      = Lwt.bind
let (=<<) f g  = Lwt.bind g f
let (>|=) f g  = Lwt.map g f
let (=|<) f g  = Lwt.map f g

module Lwt = struct
  include Lwt

  let wrapopt = function
    | Some v -> return v
    | None   -> raise_lwt Not_found

  let bind_opt m =
    bind m (function Some v -> return v | None -> raise_lwt Not_found)
end

module Lwt_io = struct
  include Lwt_io
  open Lwt_unix

  let with_connection_dns
      ?(gaiopts = [AI_FAMILY(PF_INET); AI_SOCKTYPE(SOCK_STREAM)])
      ?sock_fun node service f =
    lwt addr_infos = getaddrinfo node service gaiopts in
    lwt addr_info =
      match addr_infos
      with h::t -> Lwt.return h | [] -> raise_lwt Not_found in
    with_connection ?sock_fun addr_info.ai_addr f

  let open_connection_dns
      ?(gaiopts = [AI_FAMILY(PF_INET); AI_SOCKTYPE(SOCK_STREAM)])
      ?sock_fun node service =
    lwt addr_infos = getaddrinfo node service gaiopts in
    lwt addr_info =
      match addr_infos
      with h::t -> Lwt.return h | [] -> raise_lwt Not_found in
    open_connection ?sock_fun addr_info.ai_addr
end
