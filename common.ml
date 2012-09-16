module Currency = struct
  type t   = GBP | EUR | USD | PLN

  let to_string = function
    | GBP -> "GBP"
    | EUR -> "EUR"
    | USD -> "USD"
    | PLN -> "PLN"

  let of_string = function
    | "GBP" -> GBP
    | "EUR" -> EUR
    | "USD" -> USD
    | "PLN" -> PLN
    | _ -> failwith "Currency.of_string"
end

module Satoshi = struct
  type t = int

  let of_btc_float v = int_of_float (v *. 1e8)
  let to_btc_float v = float_of_int v /. 1e8

  let of_string = int_of_string
  let of_btc_string v = int_of_float (float_of_string v *. 1e8)
end

module Order = struct
  type kind = Bid | Ask
  type strategy =
    | Market
    | Good_till_cancelled
    | Immediate_or_cancel
    | Fill_or_kill

  type t =
      {
        direction : kind;
        stategy   : strategy option;
        currency  : Currency.t;
        price     : int; (* price in satoshis *)
        amount    : int (* amount of BTC in satoshis *)
      }
end
