(** Overlay over some Lwt functions  *)
open Lwt

val catch : (unit -> 'a t) -> exn:(exn -> 'a t) -> 'a t

val try_bind : (unit -> 'a t) -> ok:('a -> 'b t) -> exn:(exn -> 'b t) -> 'b t

val protect : (unit -> 'a t) -> finally:(unit -> unit t) -> 'a t

val don't_wait : 'a t -> unit

val failwith : string -> 'a t

val failwithf : ('a, unit, string, 'b Lwt.t) format4 -> 'a

val invalid_arg  : string -> 'a t

val invalid_argf : ('a, unit, string, 'b Lwt.t) format4 -> 'a
