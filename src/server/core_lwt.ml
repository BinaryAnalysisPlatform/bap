open Core_kernel.Std
open Core_lwt_container


module Std = struct
  module Lwt = struct
    include Lwt
    include (Core_lwt_basic : Monad with type 'a t := 'a t)
    include Core_lwt_extra
    module Main = Lwt_main
    module Unix = Lwt_unix
    module Chan = Lwt_chan
    module Mutex = Lwt_mutex
    module IO = Lwt_io
    module Or_error = Core_lwt_or_error

    module Seq = Lift_sequence(Core_lwt_basic)
    module List = struct
      include Lift_list(Core_lwt_basic)
      let partition_tf ?(how = `Sequential) t ~f =
        match how with
        | `Sequential -> Lwt_list.partition_s f t
        | `Parallel   -> Lwt_list.partition_p f t
    end
  end
  include Core_lwt_basic
  let (>>=?) = Lwt.Or_error.(>>=)
  let (>>|?) = Lwt.Or_error.(>>|)
end
