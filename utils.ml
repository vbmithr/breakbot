external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external ($)  : ('a -> 'b) -> 'a -> 'b = "%apply"
let (++) f g x = f (g x)
let (|)        = (lor)
let (&)        = (land)

module Int32 = struct
  include Int32

  (** Adding convenient operators like in Z *)
  let (+)    = add
  let (-)    = sub
  let ( * )  = mul
  let (/)    = div
  let (lsr)  = shift_right_logical
  let (lsl)  = shift_left
  let (|)    = logor
  let (&)    = logand
end

module Int64 = struct
  include Int64

  (** Adding convenient operators like in Z *)
  let (+)    = add
  let (-)    = sub
  let ( * )  = mul
  let (/)    = div
  let (lsr)  = shift_right_logical
  let (lsl)  = shift_left
  let (|)    = logor
  let (&)    = logand
end

let i_int i    = fun (i:int) -> ()
let i_float i  = fun (i:float) -> ()
let i_string i = fun (i:string) -> ()

module Map = struct
  module type OrderedType = sig
    include Map.OrderedType
  end

  module type S = sig
    include Map.S

    val of_bindings : (key * 'a) list -> 'a t
  end

  module Make(Ord: OrderedType) = struct
    include Map.Make(Ord)

    let of_bindings assocs =
      List.fold_left (fun acc (k,v) -> add k v acc) empty assocs
  end
end

module Set = struct
  module type OrderedType = sig
    include Set.OrderedType
  end

  module type S = sig
    include Set.S

    val of_list : elt list -> t
  end

  module Make(Ord: OrderedType) = struct
    include Set.Make(Ord)

    let of_list l =
      List.fold_left (fun acc v -> add v acc) empty l
  end
end

module IntMap = Map.Make
  (struct
    type t = int
    let compare = Pervasives.compare
   end)
module Int64Map = Map.Make(Int64)
module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(* "finally" is a lwt keyword... *)
let with_finally f f_block =
  try
    let res = f () in f_block (); res
  with e ->
    f_block (); raise e

module Unix = struct
  include Unix

  let gettimeofday_int () = int_of_float $ gettimeofday ()
  let gettimeofday_str () = Printf.sprintf "%.0f" $ gettimeofday ()

  let getmicrotime () = gettimeofday () *. 1e6
  let getmicrotime_int64 () = Int64.of_float $ gettimeofday () *. 1e6
  let getmicrotime_str () = Printf.sprintf "%.0f" $ gettimeofday () *. 1e6
end

module String = struct
  include String

  let is_int str =
    try let (_:int) = int_of_string str in true with _ -> false

  let is_float str =
    try let (_:float) = float_of_string str in true with _ -> false

  let of_file fname =
    let ic = open_in fname in
    let ic_len = in_channel_length ic in
    let buf = String.create ic_len in
    let rec input_forever len pos =
      if len = 0 then buf
      else let read = input ic buf pos len
           in input_forever (len-read) (pos+read) in
    with_finally
      (fun () -> input_forever ic_len 0)
      (fun () -> close_in ic)
end
