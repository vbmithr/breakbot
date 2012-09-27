(** Access configuration file *)

let of_file filename =
  let config_json = Yojson.Safe.from_file filename in
  match config_json with
    | `Assoc l ->
      (List.map (fun (id, params) ->
        match params with
          | `List p ->
            id, (match params with
              | `List p -> List.map
                (function
                | `String str -> str
                | _ -> failwith "Config.of_file") p
              | _ -> failwith "Config.of_file")
          | _ -> failwith "Config.of_file")
         l)
    | _ -> failwith "Config.of_file"
