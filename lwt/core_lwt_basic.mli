open Core_kernel.Std
type 'a t = 'a Lwt.t
include Monad with type 'a t := 'a Lwt.t
