(** IDA integration.

    This module provides an experimental integration with IDA,
    just as a proof of concept. It runs IDA in a batch mode, to
    obtain database, then runs a script on database that extract
    symbol

*)
module Std : sig

  type ida

  type 'a command

  (** Interaction with ida instance  *)
  module Ida : sig
    (** exception External_command_failed occurs when the external IDA
        command was not executed successfully *)
    exception Failed of string

    exception Not_in_path


    (** IDA instance *)
    type t = ida

    (** [create target] create an IDA instance that will work with
        [target] executable. *)
    val create : string -> t

    val exec : t -> 'a command -> 'a

    (** [close ida] finish interaction with IDA and clean all resources *)
    val close : t -> unit


    (** [with_file target analysis] creates ida instance on [target],
        perform [analysis] and close [ida] *)
    val with_file : string -> 'a command -> 'a


    (** [Ida.exec ida get_symbols] extract symbols from binary *)
    val get_symbols :  (string * int64 * int64) list command
  end


  module Command : sig
    type 'a t = 'a command

    val create : [`python | `idc] -> script:string -> process:(string -> 'a) -> 'a t
  end
end
