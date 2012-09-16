module Currency = struct
    include Common.Currency
      
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
end

