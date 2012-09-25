class mtgox =
object (self)
  inherit Exchange.exchange "mtgox"

  method update () =
    let rec update (ic, oc) =
      lwt line = Lwt_io.read_line ic in
      lwt () = Lwt_io.printf "%s\n" line in
      lwt () = self#notify () in
      update (ic, oc)
    in
    Websocket.with_websocket "http://websocket.mtgox.com/mtgox" update

  method bid curr price amount = Lwt.return ()
  method ask curr price amount = Lwt.return ()
end
