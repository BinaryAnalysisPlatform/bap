(** IDA integration.

    This module provides for a interface to ingegrate with IDA, by
    running IDA in batch mode, obtain database, run script on
    database.

    Plugins can be written to provide this service, or to use the
    service provided by other plugins (usually provided by
    plugins/ida) through this interface.
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

  end

  (** Commands that can be passed into an IDA session *)
  module Command : sig
    type 'a t = 'a command

    type language = [`python | `idc]

    val create : language -> script:string -> parser:(string -> 'a) -> 'a t

    val language : 'a t -> language

    val script : 'a t -> string

    val parser : 'a t -> (string -> 'a)
  end

  (** Allow plugins to specify that they can provide IDA service *)
  module Service : sig
    type t = {
      exec  : 'a. 'a command -> 'a;
      close : unit -> unit;
    }

    (** [provide creator] provides for a service that can perform the
        roles of [Ida.create], [Ida.exec], [Ida.close] *)
    val provide : (string -> t) -> unit
  end
end
