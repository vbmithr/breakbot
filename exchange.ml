open Utils
open Common

class virtual exchange (name:string) =
object (self)
  val mutable books = Books.empty
  val mvar = Lwt_mvar.create_empty ()

  method name      = name
  method print     = Printf.printf "Books for exchange %s:\n%!" name;
    Books.print books
  method notify    = Lwt_mvar.put mvar self

  method get_books = books
  method get_mvar  = mvar

  (** The first parameter indicates if there is an independant
      orderbook for all currencies *)
  method virtual currs     : StringSet.t
  method virtual base_curr : string

  method virtual update    : unit Lwt.t
  method virtual place_order : Order.kind -> string ->
    S.t -> S.t -> unit Lwt.t
  method virtual withdraw_btc : S.t -> string -> unit Lwt.t
  method virtual get_balances : ((string * S.t) list) Lwt.t
end
