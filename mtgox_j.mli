(* Auto-generated from "mtgox.atd" *)


type depth = Mtgox_t.depth = {
  currency: string;
  item: string;
  now: string;
  price: string;
  price_int: string;
  total_volume_int: string;
  type_: int;
  type_str: string;
  volume: string;
  volume_int: string
}

type depth_msg = Mtgox_t.depth_msg = {
  channel: string;
  depth: depth;
  op: string;
  origin: string;
  private_: string
}

val write_depth :
  Bi_outbuf.t -> depth -> unit
  (** Output a JSON value of type {!depth}. *)

val string_of_depth :
  ?len:int -> depth -> string
  (** Serialize a value of type {!depth}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_depth :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> depth
  (** Input JSON data of type {!depth}. *)

val depth_of_string :
  string -> depth
  (** Deserialize JSON data of type {!depth}. *)

val write_depth_msg :
  Bi_outbuf.t -> depth_msg -> unit
  (** Output a JSON value of type {!depth_msg}. *)

val string_of_depth_msg :
  ?len:int -> depth_msg -> string
  (** Serialize a value of type {!depth_msg}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_depth_msg :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> depth_msg
  (** Input JSON data of type {!depth_msg}. *)

val depth_msg_of_string :
  string -> depth_msg
  (** Deserialize JSON data of type {!depth_msg}. *)

