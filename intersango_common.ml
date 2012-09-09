let (|>) f g = g f

module Currency = struct
  type t   = GBP | EUR | USD | PLN

  let to_int = function
    | GBP -> 1
    | EUR -> 2
    | USD -> 3
    | PLN -> 4

  let of_int = function
    | 1 -> GBP
    | 2 -> EUR
    | 3 -> USD
    | 4 -> PLN
    | _ -> failwith "Currency.of_int"

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
      price     : float;
      amount    : float
    }
end
