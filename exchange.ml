open Common

class virtual exchange mvar =
object (self)
  val books = Books.empty ()

  method print = Books.print books
  method notify () = Lwt_mvar.put mvar true

  method virtual name   : string
  method virtual update : unit -> unit Lwt.t
  method virtual bid    : Currency.t -> int -> int -> unit Lwt.t
  method virtual ask    : Currency.t -> int -> int -> unit Lwt.t
end
