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

  type event += Progress of {
      task  : string;
      note  : string option;
      stage : int option;
      total : int option;
    }

  val progress : ?note:string -> ?stage:int -> ?total:int -> string -> unit
  val message :  level -> section:string -> ('a,Format.formatter,unit) format -> 'a


  module Create() : sig
    open Format

    val debug   : ('a,formatter,unit) format -> 'a
    val info    : ('a,formatter,unit) format -> 'a
    val warning : ('a,formatter,unit) format -> 'a
    val error   : ('a,formatter,unit) format -> 'a

    val report_progress :
      ?task:string ->
      ?note:string ->
      ?stage:int ->
      ?total:int -> unit -> unit

    val debug_formatter : formatter
    val info_formatter : formatter
    val warning_formatter : formatter
    val error_formatter : formatter
  end

end

val pp : Format.formatter -> t -> unit
