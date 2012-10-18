module CoUnix = Cohttp_lwt_unix

let post_form ?headers ~params uri =
  let open Cohttp in
  let headers = Header.add_opt headers "content-type" "application/x-www-form-urlencoded" in
  let q = List.map (fun (k,v) -> k, [v]) (Header.to_list params) in
  let body = CoUnix.Body.body_of_string (Uri.encoded_of_query q) in
  lwt body_len, body = CoUnix.Body.get_length body in
  let headers = Header.add headers "content-length" (string_of_int body_len) in
  CoUnix.Client.post ~headers ?body uri
