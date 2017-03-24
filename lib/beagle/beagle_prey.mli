open Core_kernel.Std
open Bap.Std
open Primus.Std

module Words : sig
  type t = String.Set.t
  include Value.S with type t := t
  val to_string : t -> string
end
type words = Words.t


type t

val caught : (t * words) Observation.t



(** made when a prey is detected  *)
val detected : t Observation.t


(** a statement that a prey was detected  *)
val finished : t Observation.statement

val catch : (t * words) Observation.statement

(** [create terms chars] the result of beagle hunting stating that
    during the execution of a sequence of [terms] we observed [chars]
    in that specific order.  *)
val create : tid seq -> string -> t


(** [data prey] is a sequence of chars, that was caught  *)
val data  : t -> string


(** [terms prey] a sequence of terms that, when executed, consumed
    or produced one of the [data prey] characters. *)
val terms : t -> tid seq


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
