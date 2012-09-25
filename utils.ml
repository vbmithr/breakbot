let (|>) f g = g f
let (>>=) = Lwt.bind

module IntMap = Map.Make(
  struct
    type t = int
    let compare = Pervasives.compare
  end)

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
    lwt line = Lwt_io.read_line ic in
    lwt () = Lwt_io.printf "%s\n" line in
    print_to_stdout ()
  in print_to_stdout ()

module Random = struct
  include Random

  let char () = Char.chr (Random.bits () land 1 lsl 8 - 1)

  let string size =
    let size = if size mod 2 <> 0 then size+1 else size in
    let buf = Lwt_bytes.create size in
    let oc  = Lwt_io.of_bytes Lwt_io.output buf in
    lwt () =
      for_lwt i = 0 to size/2 do
        Lwt_io.BE.write_int16 oc (Random.bits ())
      done in
    lwt () = Lwt_io.close oc
    in Lwt.return (Lwt_bytes.to_string buf)
end

module Int32 = struct
  include Int32

  let get_char int32 = function
    | 0 -> Char.chr (Int32.to_int int32 land (1 lsl 8 - 1))
    | 1 -> Char.chr (Int32.to_int (Int32.shift_right_logical int32 8)
                     land (1 lsl 8 - 1))
    | 2 -> Char.chr (Int32.to_int (Int32.shift_right_logical int32 16)
                     land (1 lsl 8 - 1))
    | 3 -> Char.chr (Int32.to_int (Int32.shift_right_logical int32 24)
                     land (1 lsl 8 - 1))

    | _ -> failwith "Int32.get_char"
end

module Int64 = struct
  include Int64
end

module String = struct
  include String

  module BE = struct
    let read_int16 buf off =
      Char.code buf.[off] lsl 8 land Char.code buf.[off+1]

    let read_int32 buf off =
      let a = Int32.shift_left (Int32.of_int (Char.code buf.[off])) 24 in
      let b = Int32.shift_left (Int32.of_int (Char.code buf.[off+1])) 16 in
      let c = Int32.shift_left (Int32.of_int (Char.code buf.[off+2])) 8 in
      let d = Int32.of_int (Char.code buf.[off+3]) in
      Int32.logand (Int32.logand (Int32.logand c d) b) a

    let write_int16 buf off i =
      buf.[off] <- Char.chr ((i lsr 8) land (1 lsl 8 - 1));
      buf.[off+1] <- Char.chr (i land (1 lsl 8 - 1))

    let write_int32 buf off i =
      buf.[off] <- Int32.get_char i 3;
      buf.[off+1] <- Int32.get_char i 2;
      buf.[off+2] <- Int32.get_char i 1;
      buf.[off+3] <- Int32.get_char i 0
  end

  module LE = struct
    let read_int16 buf off =
      Char.code buf.[off+1] lsl 8 land Char.code buf.[off]

    let read_int32 buf off =
      let a = Int32.shift_left (Int32.of_int (Char.code buf.[off+3])) 24 in
      let b = Int32.shift_left (Int32.of_int (Char.code buf.[off+2])) 16 in
      let c = Int32.shift_left (Int32.of_int (Char.code buf.[off+1])) 8 in
      let d = Int32.of_int (Char.code buf.[off]) in
      Int32.logand (Int32.logand (Int32.logand c d) b) a

    let write_int16 buf off i =
      buf.[off+1] <- Char.chr ((i lsr 8) land (1 lsl 8 - 1));
      buf.[off] <- Char.chr (i land (1 lsl 8 - 1))

    let write_int32 buf off i =
      buf.[off] <- Int32.get_char i 0;
      buf.[off+1] <- Int32.get_char i 1;
      buf.[off+2] <- Int32.get_char i 2;
      buf.[off+3] <- Int32.get_char i 3
  end

end
