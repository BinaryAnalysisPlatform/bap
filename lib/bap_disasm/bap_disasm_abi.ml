open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_ir

(** Base type definitions for ABI.

    ABI can be written from scratch, or derived from the existing
    one. New ABI can be registered in the system using
    [Target.ABI.register] function, where [Target] refers to a
    specific architecture, e.g., [ARM], [AMD64], etc.

    To obtain a list of available ABI for a given function symbol, use
    [Target.ABI.create] function. If you're lucky it will return a
    singleton list. Otherwise, you may find [ABI] module helpful.

*)

(** Application Binary Interface.

    Under this name, we're gathering several different concepts, like:

    - calling convention
    - stack frame organization
    - data representation
    - special functions

    Later we may extend the ABI class to handle system calls, type
    inference and other stuff.


    Each ABI object is constructed specifically to a particular
    symbol using the following functional constructor, of the
    following type:

    [symtab -> sym -> abi option]

    ABI constructors are registered in the target specific lifter,
    and constructed for each symbol. Afterwards a set of most
    (and equally) applicable ABIs is provided to a calling part,
    to which it is left the final decision on how to disambiguate
    them. *)
class type abi = object
  (** unique identifier of the ABI.
      Used to communicate between to ABI's.

      The order of id parts should be from more specific, to less
      specific, i.e. in reverse order (so that deriving classes can
      easily append their own parts). The architecture shouldn't be
      specified in the id, as two ABIs from different architectures
      should never met.

      A good start whould be to use:
      [specific; compiler; os; vendor]

      Example: ["*exit"; "gnueabi"; "linux"; "unknown"]

      Will encode an ABI of [exit] family of functions for ARM linux
      gnueabi. The recommended printing format for the ABI is to
      append the arch name and print all constituents of the name from
      right to left, using "-" symbol as a separator.

      In any case, the meaning of the identifier is specific to a
      particular family of ABIs, that are, usually inherit the same
      parent or set of parents. *)
  method id : string list

  (** [self#specific] is [true] if this ABI is specific
      for the provided function. The [specific] ABI is always more
      preferrable to non-specific one. If more than one specific
      ABIs is applicable for the provided symbol, than the normal
      resolution process will be used (see method [choose])
  *)
  method specific : bool

  (** [self#choose other] used to sort a set of applicable ABI.

      Must return:
      - [0] if [other] abi is not known or is considered equaly
        applicable for the given context.
      - [1] if [other] abi is known, and [self] is preferrable
        to [other]
      - [-1] if [other] abi is more preferrable. This value can
        be even returned when the other abi is not known to [self].

      In case of inconsistency the solving mechanism will consider
      inconsistent abi's as equal. The examples of inconsistent
      comparison results are: both abis preferred each other, or
      both abis claimed that they are preferrable. *)
  method choose : abi -> int

  (** [return_value] returns an expression, that can be used to return
      a value from a function. Use [Bil.concat] to represent return
      value that doesn't fit into one register  *)
  method return_value : (var * exp) option

  (** [args] returns a list of expressions that represents
      arguments of the given function. Each expression can be
      annotated with suggested name  *)
  method args : (var * exp) list

  (** [vars] returns a list of expressions, that represents
      local variables of the function  *)
  method vars : (var * exp) list

  (** [records] returns a list of records, found in the symbol.  *)
  method records : (var * exp) list list
end

(** symbol name may be provided if known. Also an access
    to the whole binary image is provided if there is one. *)
type abi_constructor = sub term -> abi
