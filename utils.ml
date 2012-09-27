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


module Uint8 = struct
  type t = int

  let min = 0
  let max = 255
end

module String = struct
  include String

  module BE = struct
    let of_int32 int32 =
      let str = String.create 4 in
      str.[0] <- Char.chr (Int32.to_int (Int32.shift_right_logical int32 24)
                           land Uint8.max);
      str.[1] <- Char.chr (Int32.to_int (Int32.shift_right_logical int32 16)
                           land Uint8.max);
      str.[2] <- Char.chr (Int32.to_int (Int32.shift_right_logical int32 8)
                           land Uint8.max);
      str.[3] <- Char.chr (Int32.to_int int32 land Uint8.max);
      str

    let read_int16 buf off =
      Char.code buf.[off] lsl 8 land Char.code buf.[off+1]

    let read_int32 buf off =
      let a = Int32.shift_left (Int32.of_int (Char.code buf.[off])) 24 in
      let b = Int32.shift_left (Int32.of_int (Char.code buf.[off+1])) 16 in
      let c = Int32.shift_left (Int32.of_int (Char.code buf.[off+2])) 8 in
      let d = Int32.of_int (Char.code buf.[off+3]) in
      Int32.logand (Int32.logand (Int32.logand c d) b) a

    let write_int16 buf off i =
      buf.[off] <- Char.chr ((i lsr 8) land Uint8.max);
      buf.[off+1] <- Char.chr (i land Uint8.max)

    let write_int32 buf off i =
      let src = of_int32 i in String.blit src 0 buf off 4
  end

  module LE = struct
    let of_int32 int32 =
      let str = String.create 4 in
      str.[3] <- Char.chr (Int32.to_int (Int32.shift_right_logical int32 24)
                           land Uint8.max);
      str.[2] <- Char.chr (Int32.to_int (Int32.shift_right_logical int32 16)
                           land Uint8.max);
      str.[1] <- Char.chr (Int32.to_int (Int32.shift_right_logical int32 8)
                           land Uint8.max);
      str.[0] <- Char.chr (Int32.to_int int32 land Uint8.max);
      str

    let read_int16 buf off =
      Char.code buf.[off+1] lsl 8 land Char.code buf.[off]

    let read_int32 buf off =
      let a = Int32.shift_left (Int32.of_int (Char.code buf.[off+3])) 24 in
      let b = Int32.shift_left (Int32.of_int (Char.code buf.[off+2])) 16 in
      let c = Int32.shift_left (Int32.of_int (Char.code buf.[off+1])) 8 in
      let d = Int32.of_int (Char.code buf.[off]) in
      Int32.logand (Int32.logand (Int32.logand c d) b) a

    let write_int16 buf off i =
      buf.[off+1] <- Char.chr ((i lsr 8) land Uint8.max);
      buf.[off] <- Char.chr (i land Uint8.max)

    let write_int32 buf off i =
      let src = of_int32 i in String.blit src 0 buf off 4
  end
end

module Random = struct
  include Random

  let char () = Char.chr (Random.bits () land Uint8.max)

  let int16_as_string () =
    let str = String.create 2 in
    String.BE.write_int16 str 0 (Random.bits ()); str

  let int32_as_string () =
    let str = String.create 4 in
    String.BE.write_int32 str 0 (Int32.of_int (Random.bits ())); str

  let string size =
    let str = String.create size in
    String.iteri (fun i _ -> str.[i] <- char ()) str;
    str
end
