(* Auto-generated from "mtgox.atd" *)


type depth = Mtgox_t.depth = {
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

type depth_msg = Mtgox_t.depth_msg = {
  channel: string;
  depth: depth;
  op: string;
  origin: string;
  private_: string
}

let write_depth = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"currency\":";
    (
      Yojson.Safe.write_string
    )
      ob x.currency;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"item\":";
    (
      Yojson.Safe.write_string
    )
      ob x.item;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"now\":";
    (
      Yojson.Safe.write_string
    )
      ob x.now;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"price\":";
    (
      Yojson.Safe.write_string
    )
      ob x.price;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"price_int\":";
    (
      Yojson.Safe.write_string
    )
      ob x.price_int;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"total_volume_int\":";
    (
      Yojson.Safe.write_string
    )
      ob x.total_volume_int;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"type\":";
    (
      Yojson.Safe.write_int
    )
      ob x.type_;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"type_str\":";
    (
      Yojson.Safe.write_string
    )
      ob x.type_str;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"volume\":";
    (
      Yojson.Safe.write_string
    )
      ob x.volume;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"volume_int\":";
    (
      Yojson.Safe.write_string
    )
      ob x.volume_int;
    Bi_outbuf.add_char ob '}';
)
let string_of_depth ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_depth ob x;
  Bi_outbuf.contents ob
let read_depth = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let x =
      {
        currency = Obj.magic 0.0;
        item = Obj.magic 0.0;
        now = Obj.magic 0.0;
        price = Obj.magic 0.0;
        price_int = Obj.magic 0.0;
        total_volume_int = Obj.magic 0.0;
        type_ = Obj.magic 0.0;
        type_str = Obj.magic 0.0;
        volume = Obj.magic 0.0;
        volume_int = Obj.magic 0.0;
      }
    in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 3 -> (
                if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'w' then (
                  2
                )
                else (
                  -1
                )
              )
            | 4 -> (
                match String.unsafe_get s pos with
                  | 'i' -> (
                      if String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'm' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | 't' -> (
                      if String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' then (
                        6
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'e' then (
                  3
                )
                else (
                  -1
                )
              )
            | 6 -> (
                if String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'e' then (
                  8
                )
                else (
                  -1
                )
              )
            | 8 -> (
                match String.unsafe_get s pos with
                  | 'c' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'c' && String.unsafe_get s (pos+7) = 'y' then (
                        0
                      )
                      else (
                        -1
                      )
                    )
                  | 't' -> (
                      if String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'r' then (
                        7
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 9 -> (
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 't' then (
                  4
                )
                else (
                  -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 't' then (
                  9
                )
                else (
                  -1
                )
              )
            | 16 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'v' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = 'u' && String.unsafe_get s (pos+10) = 'm' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 'n' && String.unsafe_get s (pos+15) = 't' then (
                  5
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            let v =
              (
                Ag_oj_run.read_string
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            let v =
              (
                Ag_oj_run.read_string
              ) p lb
            in
            Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            bits0 := !bits0 lor 0x2;
          | 2 ->
            let v =
              (
                Ag_oj_run.read_string
              ) p lb
            in
            Obj.set_field (Obj.repr x) 2 (Obj.repr v);
            bits0 := !bits0 lor 0x4;
          | 3 ->
            let v =
              (
                Ag_oj_run.read_string
              ) p lb
            in
            Obj.set_field (Obj.repr x) 3 (Obj.repr v);
            bits0 := !bits0 lor 0x8;
          | 4 ->
            let v =
              (
                Ag_oj_run.read_string
              ) p lb
            in
            Obj.set_field (Obj.repr x) 4 (Obj.repr v);
            bits0 := !bits0 lor 0x10;
          | 5 ->
            let v =
              (
                Ag_oj_run.read_string
              ) p lb
            in
            Obj.set_field (Obj.repr x) 5 (Obj.repr v);
            bits0 := !bits0 lor 0x20;
          | 6 ->
            let v =
              (
                Ag_oj_run.read_int
              ) p lb
            in
            Obj.set_field (Obj.repr x) 6 (Obj.repr v);
            bits0 := !bits0 lor 0x40;
          | 7 ->
            let v =
              (
                Ag_oj_run.read_string
              ) p lb
            in
            Obj.set_field (Obj.repr x) 7 (Obj.repr v);
            bits0 := !bits0 lor 0x80;
          | 8 ->
            let v =
              (
                Ag_oj_run.read_string
              ) p lb
            in
            Obj.set_field (Obj.repr x) 8 (Obj.repr v);
            bits0 := !bits0 lor 0x100;
          | 9 ->
            let v =
              (
                Ag_oj_run.read_string
              ) p lb
            in
            Obj.set_field (Obj.repr x) 9 (Obj.repr v);
            bits0 := !bits0 lor 0x200;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 3 -> (
                  if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'w' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 4 -> (
                  match String.unsafe_get s pos with
                    | 'i' -> (
                        if String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'm' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | 't' -> (
                        if String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' then (
                          6
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'e' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  if String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'e' then (
                    8
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  match String.unsafe_get s pos with
                    | 'c' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'c' && String.unsafe_get s (pos+7) = 'y' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | 't' -> (
                        if String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'r' then (
                          7
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 't' then (
                    4
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 't' then (
                    9
                  )
                  else (
                    -1
                  )
                )
              | 16 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'v' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = 'u' && String.unsafe_get s (pos+10) = 'm' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 'n' && String.unsafe_get s (pos+15) = 't' then (
                    5
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              let v =
                (
                  Ag_oj_run.read_string
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              let v =
                (
                  Ag_oj_run.read_string
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              bits0 := !bits0 lor 0x2;
            | 2 ->
              let v =
                (
                  Ag_oj_run.read_string
                ) p lb
              in
              Obj.set_field (Obj.repr x) 2 (Obj.repr v);
              bits0 := !bits0 lor 0x4;
            | 3 ->
              let v =
                (
                  Ag_oj_run.read_string
                ) p lb
              in
              Obj.set_field (Obj.repr x) 3 (Obj.repr v);
              bits0 := !bits0 lor 0x8;
            | 4 ->
              let v =
                (
                  Ag_oj_run.read_string
                ) p lb
              in
              Obj.set_field (Obj.repr x) 4 (Obj.repr v);
              bits0 := !bits0 lor 0x10;
            | 5 ->
              let v =
                (
                  Ag_oj_run.read_string
                ) p lb
              in
              Obj.set_field (Obj.repr x) 5 (Obj.repr v);
              bits0 := !bits0 lor 0x20;
            | 6 ->
              let v =
                (
                  Ag_oj_run.read_int
                ) p lb
              in
              Obj.set_field (Obj.repr x) 6 (Obj.repr v);
              bits0 := !bits0 lor 0x40;
            | 7 ->
              let v =
                (
                  Ag_oj_run.read_string
                ) p lb
              in
              Obj.set_field (Obj.repr x) 7 (Obj.repr v);
              bits0 := !bits0 lor 0x80;
            | 8 ->
              let v =
                (
                  Ag_oj_run.read_string
                ) p lb
              in
              Obj.set_field (Obj.repr x) 8 (Obj.repr v);
              bits0 := !bits0 lor 0x100;
            | 9 ->
              let v =
                (
                  Ag_oj_run.read_string
                ) p lb
              in
              Obj.set_field (Obj.repr x) 9 (Obj.repr v);
              bits0 := !bits0 lor 0x200;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3ff then Ag_oj_run.missing_fields [| !bits0 |] [| "currency"; "item"; "now"; "price"; "price_int"; "total_volume_int"; "type_"; "type_str"; "volume"; "volume_int" |];
        Ag_oj_run.identity x
      )
)
let depth_of_string s =
  read_depth (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_depth_msg = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"channel\":";
    (
      Yojson.Safe.write_string
    )
      ob x.channel;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"depth\":";
    (
      write_depth
    )
      ob x.depth;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"op\":";
    (
      Yojson.Safe.write_string
    )
      ob x.op;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"origin\":";
    (
      Yojson.Safe.write_string
    )
      ob x.origin;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"private\":";
    (
      Yojson.Safe.write_string
    )
      ob x.private_;
    Bi_outbuf.add_char ob '}';
)
let string_of_depth_msg ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_depth_msg ob x;
  Bi_outbuf.contents ob
let read_depth_msg = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let x =
      {
        channel = Obj.magic 0.0;
        depth = Obj.magic 0.0;
        op = Obj.magic 0.0;
        origin = Obj.magic 0.0;
        private_ = Obj.magic 0.0;
      }
    in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 2 -> (
                if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 'p' then (
                  2
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'h' then (
                  1
                )
                else (
                  -1
                )
              )
            | 6 -> (
                if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' then (
                  3
                )
                else (
                  -1
                )
              )
            | 7 -> (
                match String.unsafe_get s pos with
                  | 'c' -> (
                      if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'l' then (
                        0
                      )
                      else (
                        -1
                      )
                    )
                  | 'p' -> (
                      if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'v' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'e' then (
                        4
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            let v =
              (
                Ag_oj_run.read_string
              ) p lb
            in
            Obj.set_field (Obj.repr x) 0 (Obj.repr v);
            bits0 := !bits0 lor 0x1;
          | 1 ->
            let v =
              (
                read_depth
              ) p lb
            in
            Obj.set_field (Obj.repr x) 1 (Obj.repr v);
            bits0 := !bits0 lor 0x2;
          | 2 ->
            let v =
              (
                Ag_oj_run.read_string
              ) p lb
            in
            Obj.set_field (Obj.repr x) 2 (Obj.repr v);
            bits0 := !bits0 lor 0x4;
          | 3 ->
            let v =
              (
                Ag_oj_run.read_string
              ) p lb
            in
            Obj.set_field (Obj.repr x) 3 (Obj.repr v);
            bits0 := !bits0 lor 0x8;
          | 4 ->
            let v =
              (
                Ag_oj_run.read_string
              ) p lb
            in
            Obj.set_field (Obj.repr x) 4 (Obj.repr v);
            bits0 := !bits0 lor 0x10;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 2 -> (
                  if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 'p' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'h' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 7 -> (
                  match String.unsafe_get s pos with
                    | 'c' -> (
                        if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'l' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | 'p' -> (
                        if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'v' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'e' then (
                          4
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              let v =
                (
                  Ag_oj_run.read_string
                ) p lb
              in
              Obj.set_field (Obj.repr x) 0 (Obj.repr v);
              bits0 := !bits0 lor 0x1;
            | 1 ->
              let v =
                (
                  read_depth
                ) p lb
              in
              Obj.set_field (Obj.repr x) 1 (Obj.repr v);
              bits0 := !bits0 lor 0x2;
            | 2 ->
              let v =
                (
                  Ag_oj_run.read_string
                ) p lb
              in
              Obj.set_field (Obj.repr x) 2 (Obj.repr v);
              bits0 := !bits0 lor 0x4;
            | 3 ->
              let v =
                (
                  Ag_oj_run.read_string
                ) p lb
              in
              Obj.set_field (Obj.repr x) 3 (Obj.repr v);
              bits0 := !bits0 lor 0x8;
            | 4 ->
              let v =
                (
                  Ag_oj_run.read_string
                ) p lb
              in
              Obj.set_field (Obj.repr x) 4 (Obj.repr v);
              bits0 := !bits0 lor 0x10;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1f then Ag_oj_run.missing_fields [| !bits0 |] [| "channel"; "depth"; "op"; "origin"; "private_" |];
        Ag_oj_run.identity x
      )
)
let depth_msg_of_string s =
  read_depth_msg (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
