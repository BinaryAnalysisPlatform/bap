(** Name demangling.

    This library provides an interface for creating and registering
    demanglers, that can be used on demand by fronends and plugins.

 *)
open Core_kernel.Std

module Std : sig
  type demangler


  (** Demangler is a named string transformation.  *)
  module Demangler : sig
    type t = demangler


    (** [create name demangler]  *)
    val create : string -> (string -> string) -> t

    (** [run demangler name] demangle given [name] *)
    val run : t -> string -> string


    (** [name demangler] returns a [demangler]'s name.  *)
    val name : t -> string
  end


  (** Registry of demanglers.  *)
  module Demanglers : sig

    (** [register demangler] register new demangler.  *)
    val register : demangler -> unit

    (** [available ()] lists currently registered demanglers.  *)
    val available : unit -> demangler list
  end
end
