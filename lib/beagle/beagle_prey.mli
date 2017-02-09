open Core_kernel.Std
open Bap.Std

module Words : sig
  type t = String.Set.t
  include Value.S with type t := t
  val to_string : t -> string
end
type words = Words.t

(** Attributes that are added by beagle analysis.  *)

(** Each string in a set is a sequence of characters
    that were detected by Beagle on each emulation (it is possible,
    that beagle will sniff the same term more than once).
    The characters are specified in an order in which they were
    observed.*)
val chars : words tag

(** a set of static strings that we directly or indirectly referenced
    the emulation of a term.*)
val strings : words tag

(** a set of words that can be built from a specified alphabet with
    the observed characters. *)
val words : words tag
