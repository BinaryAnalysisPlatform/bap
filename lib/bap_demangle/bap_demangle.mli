(** Name demangling.

    Name demanglers perfrom ABI-specific name translations that
    restore the original source-language names of symbols, like
    removing leading underscores, or decoding C++ function names.
*)
open Core_kernel[@@warning "-D"]
open Bap_core_theory


module Std : sig
  type demangler


  (** Demangler is a named string transformation.  *)
  module Demangler : sig
    type t = demangler

    (** [create name demangler] creates and registers a new named demangler.

        A simple demangler is a total function that either demangles a
        symbol name or leaves it untouched if it can't or if the name
        is not mangled.

        @since 2.5.0 accepts the optional (but recommended) [package]
        parameter.

        @since 2.5.0 fails demangler with the given [package:name]
        already exists.

        @since 2.5.0 automatically registers the demangler in the
        repository.
    *)
    val create : ?package:string -> string -> (string -> string) -> t


    (** [define name run] creates and registers a simple named demangler.

        A simple demangler is a total function that either demangles a
        symbol name or leaves it untouched if it can't or if the name
        is not mangled.

        Essentially, [declare name run] is [ignore (create name run)].

        @since 2.5.0
    *)
    val declare : ?package:string -> string -> (string -> string) -> unit


    (** [id] the identity demangler that returns names unchanged.

        The [name id] is [bap:id]

        @since 2.5.0  *)
    val id : demangler



    (** [strip_leading_underscore] the demangler that strips one
        leading underscore.

        If a symbol is not underscored then it is returned unchanged.

        The [name strip_leading_underscore] is [bap:strip-leading-underscore]

        @since 2.5.0
    *)
    val strip_leading_underscore : demangler

    (** [run demangler name] demangle given [name]. *)
    val run : t -> string -> string

    (** [name demangler] returns [demangler]'s unqualified name as a string.  *)
    val name : t -> string

    (** [fullname d] the fully-qualified name of the demangler [d].

        @since 2.5.0 *)
    val fullname : t -> KB.Name.t
  end


  (** Registry of demanglers.  *)
  module Demanglers : sig

    (** [install t d] installs [d] as the default demangler for the
        target [t].

        The demangler will be used every time a name is decided from
        the set of possible names for a label that belongs to a
        program unit that has target [t].

        Fails if a demangler for the given target is already
        installed.

        @since 2.5.0
    *)
    val install : Theory.target -> demangler -> unit

    (** [register demangler] DEPRECATED.

        @before 2.5.0 registers new demangler.
        @after 2.5.0 no longer needed and does nothing, all demanglers
        are automatically registered on creation. *)
    val register : demangler -> unit
    [@@deprecated "since 2022-07 use [create] and/or [install]"]

    (** [lookup ?package name] lookups in the registry for the
        demangler with the given [package:name].

        @param package defaults to "user"

        @since 2.5.0   *)
    val lookup : ?package:string -> string -> demangler option


    (** [get ?package name] returns the [package:name] demangler.

        Fails with [Invalid_arg] if no such demangler exists.

        @since 2.5.0 *)
    val get : ?package:string -> string -> demangler


    (** [select t] returns the demangler installed for the target [t].

        If not demangler was installed then returns an identity
        demangler that doesn't touch names.

        @since 2.5.0  *)
    val select : Theory.Target.t -> demangler

    (** [available ()] lists currently registered demanglers.  *)
    val available : unit -> demangler list


  end
end
