open Core_kernel.Std
open Core_lwt.Std
open Bap.Std
open Rpc

(** runs an instance of BAP server. Returns a task.  *)
val run : (request,response) Transport.pipe -> unit Lwt.Or_error.t
