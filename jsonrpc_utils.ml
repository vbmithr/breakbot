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
