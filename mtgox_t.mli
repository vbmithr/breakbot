(* Auto-generated from "mtgox.atd" *)


type depth = {
  currency: string;
  item: string;
  now: string;
  price: string;
  price_int: string;
  total_volume_int: string;
  type_: int;
  type_str: string;
  volume: string;
  volume_int: string
}

type depth_msg = {
  channel: string;
  depth: depth;
  op: string;
  origin: string;
  private_: string
}
