(** Get the latest ECB rates *)

open Utils

open Cohttp_lwt_unix

let url = "http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml"

let get_rates ?(url=url) () =
  let uri = Uri.of_string url in
  lwt ret = Client.get uri in
  let resp, body = Opt.unopt ret in
  lwt xml = Body.string_of_body body in
  let xml_input = Xmlm.make_input (`String (0, xml)) in
  let rec parse acc input =
    let next = try Some (Xmlm.input input) with _ -> None in
    if next = None then acc else
      match Opt.unopt next with
        | `El_start ((_, "Cube"),
                     [((_, "currency"), curr); ((_, "rate"), rate)]) ->
          parse ((curr, float_of_string rate)::acc) input
        | _ -> parse acc input
  in
  Lwt.return (parse [] xml_input)

let get_rates_curr ?(url=url) curr =
  lwt rates = get_rates ~url () in
  if curr = "EUR" then Lwt.return (("EUR", 1.)::rates) else
    let curr_rate = List.assoc curr rates in
    Lwt.return
      (("EUR", 1. /. curr_rate) ::
          List.map (fun (s,r) -> s, (r /. curr_rate)) rates)

let make_convert rates from =
  fun curr ->
    (let rate = List.assoc curr rates in
     (fun amount -> rate *. amount))

let make_convert_Z rates from =
  fun curr ->
    (let rate = List.assoc curr rates in
     fun amount -> Z.of_float (rate *. Z.to_float amount))

let converters =
  lwt rates = get_rates_curr "USD" in
  let currencies = List.map (fun (c,_) -> c) rates in
  let convert_froms = List.map
    (fun c -> c, make_convert_Z rates c) currencies in
  let converters_funs =
    List.map (fun (c, f) -> List.map
      (fun curr -> ((c, curr), (f  curr)))
      currencies) convert_froms |> List.flatten in
  Lwt.return (fun from to_ -> List.assoc (from, to_) converters_funs)
