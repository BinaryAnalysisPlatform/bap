open Core_kernel.Std
open Regular.Std
open Bap.Std
open Bap_api

(** ABI dispatching interface.


    ABI object consolidates knowledge about a transformation from
    higher level language abstractions to a lower level
    representation, that uses bitvectors, stack frames, etc.

    The ABI resolving is split into two phases, first an ABI should
    be determined, second it is applied.

    In the resolvinig phase we're trying to figure out what ABI is
    used by a particular function. Some common architectures, e.g.,
    x86, are allowing to pinpoint a specific calling convention for a
    function, using function attributes. Some architectures have
    a different default ABI depending on a target. That's why we're
    allowing to override ABI for each function. If no resolver is
    registered for a given architecture, then the default resolver
    will be used, that will pick the first available abi for the
    architecture. If no abi is registered, then the ["unknown"] abi is
    returned.
*)


type 'a language


(** [register arch name f] registers an abi function [f] for abi with
    the given [name] and [arch]. If an abi for the given pair is
    already registered, then it will be overriden. *)

type 'a t = 'a language -> sub term -> sub term
val register : 'a language -> arch -> string -> 'a t -> unit

(** [override_resolver arch resolve] overrides resolver for the given
    architecture. For each function having a [name] and a list of
    [attrubutes] a [resolve name attributes] must evaluate to
    [abi_name]. *)
val override_resolver : 'a language -> arch -> ('a -> string) -> unit

(** [apply arch proto sub] returns a right hand side expression representing
    positional argument at the given position [pos] *)
val apply : 'a language -> arch -> 'a t

(** [known_abi arch] is a list of names of ABI currently registered for the given
    architecture [arch] *)
val known_abi : 'a language -> arch -> string list
