module type EXCHANGE : sig
  type t

  val update_book : t -> Order.kind -> Currency.t -> int -> int -> t
end
