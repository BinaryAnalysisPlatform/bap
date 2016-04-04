open Core_kernel.Std
open Regular.Std
open Bap.Std
open Bap_api

(** ABI dispatching interface.

    So far the ABI is just a mapping from argument position
    to a BIL expression, that represents a right hand side value
    of the corresponding argument.

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

type t

(** [register arch name f] registers an abi function [f] for abi with
    the given [name] and [arch]. If an abi for the given pair is
    already registered, then it will be overriden. *)
val register : arch -> string -> (pos -> exp) -> unit

(** [override_resolver arch resolve] overrides resolver for the given
    architecture. For each function having a [name] and a list of
    [attrubutes] a [resolve name attributes] must evaluate to
    [abi_name]. *)
val override_resolver : arch -> (string -> attr list -> string) -> unit

(** [resolve arch name attrs] calls the resolver to determine an abi
    for a function with the given [name] and [attributes] and then
    lookups the returned [abi_name] if the registry. If nothing found
    returns an abi that will map each position an an [unknown] expression *)
val resolve : arch -> string -> attr list -> t

(** [apply abi pos] returns a right hand side expression representing
    positional argument at the given position [pos] *)
val apply : t -> pos -> exp

(** [known_abi arch] is a list of names of ABI currently registered for the given
    architecture [arch] *)
val known_abi : arch -> string list

(** Stack abstraction.

    Most architectures use stack to pass arguments. This is a helper
    module, that implements a default stack structure.
*)
module Stack : sig
  type direction = [`up | `down]
  (** [create ?direction arch] creates a [stack] abstraction for the
      given architecture. A [stack n], evaluates to an expression that
      loads a word of size [s] from address [SP # n * s], where [s] is
      a word size for the given architecture; [#] is [+] if the stack
      grows downward and [-] if it grows upwards. The [direction]
      parameter specifies the stack growth direction and defaults to
      [`down] ward *)
  val create : ?direction:direction -> arch -> (int -> exp)
end
