open Regular.Std
open Bap_future.Std
open Bap_plugins.Std

type t = ..
type event = t = ..


val stream : t stream

val send : t -> unit


(** [register_printer f] when event [e] is printed, [f e] must be
    [None] if [f] is not a subset of events, that is intended to be
    printed by an [f]. If it is [Some str], then [str] is printed
    out.

    If more than one printer returns [Some thing] for the same event,
    then the last registered has the precedence.
*)
val register_printer : (t -> string option) -> unit

module Log : sig
  type level =
    | Debug
    | Info
    | Warning
    | Error

  type info = {
    level : level;
    section : string;
    message : string;
  }

  type event += Message of info

  val message :  level -> section:string -> ('a,Format.formatter,unit) format -> 'a
end

include Printable.S with type t := t
