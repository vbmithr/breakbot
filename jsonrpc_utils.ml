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
module CoUnix = Cohttp_lwt_unix

module Rpc = struct
  include Rpc

  let rec filter_null = function
    | Enum l -> Enum (List.map filter_null l)
    | Dict d -> Dict (
      List.map (fun (s,v) -> s, filter_null v) $
        List.filter (fun (s,v) -> v <> Null) d)
    | oth -> oth

  let rec int_to_float = function
    | Enum l -> Enum (List.map int_to_float l)
    | Dict d -> Dict (List.map (fun (s,v) -> s, int_to_float v) d)
    | Int i -> Float (Int64.to_float i)
    | oth -> oth
end

module Jsonrpc = struct
  include Jsonrpc

  let get ?(transform=fun i -> i) url =
    lwt resp, body = Lwt.bind_opt $ CoUnix.Client.get url in
    lwt body_str = CoUnix.Body.string_of_body body in
    let rpc = of_string body_str in Lwt.wrap (fun () -> transform rpc)

  let get_filter_null = get ~transform:Rpc.filter_null
  let get_int_to_float = get ~transform:Rpc.int_to_float

end
