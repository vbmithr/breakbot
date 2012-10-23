(** Access configuration file *)
open Utils

type config = (string * (string list)) list with rpc

let of_file fname =
  let str = String.of_file fname in
  config_of_rpc $ Jsonrpc.of_string str
