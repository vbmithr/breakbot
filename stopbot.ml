(*
 * Copyright (c) 2012 Vincent Bernardoff <vb@luminar.eu.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Utils
open Lwt_utils
open Common
module CoUnix = Cohttp_lwt_unix

let orders = order list ref

let rpc_server =
  let callback conn_id ?body req =
    let module R = try match Rpc_client.content_type_of_string
        (Opt.unbox $ CoUnix.Request.get_param req "content-type") with
          | `XML -> (val Xmlrpc)
          | `JSON -> (val Jsonrpc)
      with Not_found -> (val Jsonrpc)
    in
    match body with
      | Some b ->
        let body_str = CoUnix.Body.to_string b in
        let call = R.call_of_string body_str
          ()

      | None -> ()

let main () =
  let template = "$(date).$(milliseconds) [$(level)]: $(message)" in
  let std_logger =
    Lwt_log.channel ~template ~close_mode:`Keep ~channel:Lwt_io.stdout () in
  lwt file_logger = Lwt_log.file ~template ~file_name:"breakbot.log" () in
  let brd_logger = Lwt_log.broadcast [std_logger; file_logger] in
  let () = Lwt_log.default := brd_logger in
  let config = Config.of_file "breakbot.conf" in
  let mtgox_key, mtgox_secret, mtgox_addr  =
    match (List.assoc "mtgox" config) with
      | [key; secret; addr] ->
        Uuidm.to_bytes (Opt.unbox (Uuidm.of_string key)),
        Cohttp.Base64.decode secret, addr
      | _ -> failwith "Syntax error in config file."
  in
  let ustream, push_f = Lwt_stream.create () in
  let mtgox = new Mtgox.mtgox mtgox_key mtgox_secret mtgox_addr push_f


let () = Lwt_main.run $ main ()
