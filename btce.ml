module CoUnix = Cohttp_lwt_unix

open Utils
open Lwt_utils
open Common

let url = Uri.of_string "https://btc-e.com/api/2/btc_usd/depth"
let period = 2.0

module Protocol = struct
  type depth =
      { asks: (float * float) list;
        bids: (float * float) list
      } with rpc
end

class btce key secret =
object (self)
  inherit Exchange.exchange "btce"

  method currs = StringSet.of_list ["USD"]
  method base_curr = "USD"

  method update =
    let open Protocol in
    lwt resp, body = Lwt.bind_opt $ CoUnix.Client.get url in
    lwt body_str = CoUnix.Body.string_of_body body in
    let depth = depth_of_rpc $ Jsonrpc.of_string_int_to_float body_str in
    let ask_book = List.fold_left
      (fun acc d -> let price_float, amount_float = d in
                    let price, amount =
                      (S.of_face_float price_float),
                      (S.of_face_float amount_float) in
                    Book.add price amount acc) Book.empty depth.asks
    and bid_book = List.fold_left
      (fun acc d -> let price_float, amount_float = d in
                    let price, amount =
                      (S.of_face_float price_float),
                      (S.of_face_float amount_float) in
                    Book.add price amount acc) Book.empty depth.bids in
    let () = books <- StringMap.add "USD" (bid_book, ask_book) books in
    lwt () = self#notify in
    lwt () = Lwt_unix.sleep period in self#update

  method place_order kind curr price amount = Lwt.return Rpc.Null
  method withdraw_btc amount curr = Lwt.return Rpc.Null
  method get_balances = Lwt.return []
end
