open Core_kernel.Std
open Core_lwt_container_intf

type 'a t = 'a Or_error.t Lwt.t

include Monad with type 'a t := 'a t

val fail  : Error.t -> _ t
val of_exn : ?backtrace:[`Get | `This of string] -> exn -> _ t
val errorf : ('a, unit, string, _ t) format4 -> 'a
val error : string -> 'a -> ('a -> Sexp.t) -> _ t
val error_string : string -> _ t
val unimplemented : string -> _ t
val combine_errors : 'a t list -> 'a list t
val combine_errors_unit : unit t list -> unit t

val ok_unit  : unit t
val ok_true  : bool t
val ok_false : bool t
val ok_nil   : 'a list t


val try_with
  : ?backtrace:bool (** default is [false] *)
  -> (unit -> 'a Lwt.t)
  -> 'a t

val try_with_join
  : ?backtrace:bool (** default is [false] *)
  -> (unit -> 'a t)
  -> 'a t

module List : Monad_sequence with type 'a monad := 'a t
                              and type 'a t := 'a list
module Seq  : Monad_sequence with type 'a monad := 'a t
                              and type 'a t := 'a Sequence.t
