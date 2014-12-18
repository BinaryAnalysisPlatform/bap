open Core_kernel.Std

include Monad.Make(struct
    type 'a t = 'a Lwt.t
    let return = Lwt.return
    let bind = Lwt.bind
    let map m ~f = Lwt.map f m
    let map = `Custom map
  end)
