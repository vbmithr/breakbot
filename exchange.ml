open Common

class virtual exchange (name:string) =
object (self)
  val mutable books = Books.empty
  val mvar = Lwt_mvar.create_empty ()

  method name      = name
  method print     = Books.print books
  method notify    = Lwt_mvar.put mvar name
  method get_mvar  = mvar

  method virtual update : unit Lwt.t
  method virtual bid    : Currency.t -> int -> int -> unit Lwt.t
  method virtual ask    : Currency.t -> int -> int -> unit Lwt.t
end
