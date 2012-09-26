open Yojson.Safe

module Protocol = struct

  type channel = Trade | Ticker | Depth

  let channel_of_string = function
    | "24e67e0d-1cad-4cc0-9e7a-f8523ef460fe" -> Depth
    | "d5f06780-30a8-4a48-a2f8-7ed181b4a13f" -> Ticker
    | "dbf1dee9-4f2e-4a08-8cb7-748919a71b21" -> Trade
    | _ -> failwith "channel_of_string"

  let string_of_channel = function
    | Depth  -> "24e67e0d-1cad-4cc0-9e7a-f8523ef460fe"
    | Ticker -> "d5f06780-30a8-4a48-a2f8-7ed181b4a13f"
    | Trade  -> "dbf1dee9-4f2e-4a08-8cb7-748919a71b21"

  type op_type =
    | Subscribe
    | Unsubscribe
    | Remark
    | Private
    | Result
    | Call
    | MtgoxSubscribe

  let subscribe channel =
    `Assoc ["op", `String "subscribe"; "channel",
            `String (string_of_channel channel)]

  let unsubscribe channel =
    `Assoc ["op", `String "unsubscribe"; "channel",
            `String (string_of_channel channel)]

end

open Protocol

class mtgox =
object (self)
  inherit Exchange.exchange "mtgox"

  val mutable ic = Lwt_io.zero
  val mutable oc = Lwt_io.null

  method private set_ic newic = ic <- newic
  method private set_oc newoc = oc <- newoc

  method update () =
    let buf = Bi_outbuf.create 4096 in
    let rec update (ic, oc) =
      let rec main_loop () =
        lwt line = Lwt_io.read_line ic in
        lwt () = Lwt_io.printf "%s\n" line in
        lwt () = self#notify () in
        main_loop () in

      (* Initialisation: Unsubscribe from some channels *)
      self#set_ic ic;
      self#set_oc oc;
      lwt () = Lwt_io.write_line oc (Yojson.Safe.to_string ~buf
                                       (unsubscribe Ticker)) in
      main_loop () in
    Websocket.with_websocket "http://websocket.mtgox.com/mtgox" update

  method bid curr price amount = Lwt.return ()
  method ask curr price amount = Lwt.return ()
end
