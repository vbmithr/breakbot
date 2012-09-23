open Common

class virtual exchange (push: string -> unit) =
object (self)
  val books = Books.empty ()

  method print = Books.print books
  method push () = push self#name

  method virtual name   : string
  method virtual update : unit -> unit Lwt.t
  method virtual bid    : Currency.t -> int -> int -> unit Lwt.t
  method virtual ask    : Currency.t -> int -> int -> unit Lwt.t
end
