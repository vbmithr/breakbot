(* Auto-generated from "btce.atd" *)


type order_book = Btce_t.order_book = {
  bids: float list list;
  asks: float list list
}

val write_order_book :
  Bi_outbuf.t -> order_book -> unit
  (** Output a JSON value of type {!order_book}. *)

val string_of_order_book :
  ?len:int -> order_book -> string
  (** Serialize a value of type {!order_book}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_order_book :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> order_book
  (** Input JSON data of type {!order_book}. *)

val order_book_of_string :
  string -> order_book
  (** Deserialize JSON data of type {!order_book}. *)

