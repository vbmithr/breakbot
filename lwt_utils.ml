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
  open Lwt
  open Lwt_unix

  let open_connection ?buffer_size ?sock_fun sockaddr =
    let fd = Lwt_unix.socket
      (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
    let close = lazy begin
      try_lwt
        Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL;
        return ()
      with Unix.Unix_error(Unix.ENOTCONN, _, _) ->
      (* This may happen if the server closed the connection before us *)
        return ()
      finally
        Lwt_unix.close fd
    end in
    try_lwt
      lwt () = Lwt_unix.connect fd sockaddr in
      (try Lwt_unix.set_close_on_exec fd with Invalid_argument _ -> ());
      (match sock_fun with Some f -> (try f fd with _ -> ()) | None -> ());
      return (make ?buffer_size
                ~close:(fun _ -> Lazy.force close)
                ~mode:input (Lwt_bytes.read fd),
              make ?buffer_size
                ~close:(fun _ -> Lazy.force close)
                ~mode:output (Lwt_bytes.write fd))
    with exn ->
      lwt () = Lwt_unix.close fd in
      raise_lwt exn

  let with_connection_dns
      ?(gaiopts = [AI_FAMILY(PF_INET); AI_SOCKTYPE(SOCK_STREAM)])
      node service f =
    lwt addr_infos = getaddrinfo node service gaiopts in
    lwt addr_info =
      match addr_infos
      with h::t -> Lwt.return h | [] -> raise_lwt Not_found in
    Lwt_io.with_connection addr_info.ai_addr f

  let open_connection_dns
      ?(gaiopts = [AI_FAMILY(PF_INET); AI_SOCKTYPE(SOCK_STREAM)])
      ?sock_fun node service =
    lwt addr_infos = getaddrinfo node service gaiopts in
    lwt addr_info =
      match addr_infos
      with h::t -> Lwt.return h | [] -> raise_lwt Not_found in
    open_connection ?sock_fun addr_info.ai_addr
end
