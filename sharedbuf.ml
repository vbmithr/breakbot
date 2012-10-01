(** Shared buffers *)

type t = { buf: string;
           can_read: int Lwt_mvar.t;
           can_write: unit Lwt_mvar.t }

let empty ?(bufsize=Lwt_io.default_buffer_size ()) () =
  { buf = String.make bufsize '\000';
    can_read = Lwt_mvar.create_empty ();
    can_write = Lwt_mvar.create () }

let with_read buf (f : string -> int -> int Lwt.t)  =
  lwt msg_len = Lwt_mvar.take buf.can_read in
  lwt len_read = f buf.buf msg_len in
  lwt () = Lwt_mvar.put buf.can_write () in
  Lwt.return len_read

let with_write buf (f : string -> int Lwt.t) =
  lwt () = Lwt_mvar.take buf.can_write in
  lwt msg_len = f buf.buf in
  lwt () = Lwt_mvar.put buf.can_read msg_len in
  Lwt.return msg_len

(** Write as much as we can of [line] into [buf] *)
let write_line buf line =
  with_write buf (fun buf ->
    let len_written = min (String.length buf) (String.length line) in
    String.blit line 0 buf 0 len_written; Lwt.return len_written)

let write_lines buf lines =
  Lwt_list.iter_s (fun l -> lwt _ = write_line buf l in Lwt.return ()) lines

(* Unused / untested *)
let with_write_msg buf f =
  lwt msg = f () in
  let msg_len = String.length msg in
  let buf_len = String.length buf.buf in
  let rec loop off =
    if off + buf_len < msg_len then
      lwt (_:int) = with_write buf
        (fun buf -> String.blit msg off buf 0 buf_len; Lwt.return buf_len)
      in loop (off+buf_len)
    else
      with_write buf
        (fun buf -> String.blit msg off buf 0 (msg_len - off);
          Lwt.return (msg_len - off))
  in loop 0
