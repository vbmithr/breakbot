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

  let sockaddr_of_dns
      ?(gaiopts = [AI_FAMILY(PF_INET); AI_SOCKTYPE(SOCK_STREAM)])
      node service =
    (match_lwt getaddrinfo node service gaiopts with
      | h::t -> Lwt.return h
      | []   -> raise_lwt Not_found)
    >|= fun ai -> ai.ai_addr
end
