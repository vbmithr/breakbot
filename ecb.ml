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

let get_rates_dollar ?(url=url) () =
  let rates = get_rates ?url () in
  let dollar_rate = List.assoc "USD" rates in
  ("EUR", 1 /. dollar_rate) :: List.map (fun (_,r) -> r /. dollar_rate) rates
