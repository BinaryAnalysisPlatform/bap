(** Declarations of various ARM targets.*)

open Bap_core_theory


(** The parent of all ARM targets.

    When a new target is declared it is advised to use this target as
    parent so that the newly declared target will be included into the
    ARM Targets family.
    The [parent] target is pure abstract and doesn't have any
    propreties set.
*)
val parent : Theory.Target.t


(** The family of little endian targets.

    Each version is the parent to the following version, with [parent]
    being the the earliest version.*)
module LE : sig
  val parent : Theory.Target.t (** currently the same as [v4]  *)
  val v4 : Theory.Target.t
  val v4t : Theory.Target.t
  val v5 : Theory.Target.t
  val v5t : Theory.Target.t
  val v5te : Theory.Target.t
  val v5tej : Theory.Target.t
  val v6 : Theory.Target.t
  val v6t2 : Theory.Target.t
  val v6z : Theory.Target.t
  val v6k : Theory.Target.t
  val v6m : Theory.Target.t
  val v7 : Theory.Target.t
  val v7fp : Theory.Target.t
  val v7a : Theory.Target.t
  val v7afp : Theory.Target.t
  val v8a : Theory.Target.t
  val v81a : Theory.Target.t
  val v82a : Theory.Target.t
  val v83a : Theory.Target.t
  val v84a : Theory.Target.t
  val v85a : Theory.Target.t
  val v86a : Theory.Target.t
end


(** The family of big endian targets.

    Each version is the parent to the following version, with [parent]
    being the the earliest version.*)
module EB : sig
  val parent : Theory.Target.t (** currently the same as [v4]  *)
  val v4 : Theory.Target.t
  val v4t : Theory.Target.t
  val v5 : Theory.Target.t
  val v5t : Theory.Target.t
  val v5te : Theory.Target.t
  val v5tej : Theory.Target.t
  val v6 : Theory.Target.t
  val v6t2 : Theory.Target.t
  val v6z : Theory.Target.t
  val v6k : Theory.Target.t
  val v6m : Theory.Target.t
  val v7 : Theory.Target.t
  val v7fp : Theory.Target.t
  val v7a : Theory.Target.t
  val v7afp : Theory.Target.t
  val v8a : Theory.Target.t
  val v81a : Theory.Target.t
  val v82a : Theory.Target.t
  val v83a : Theory.Target.t
  val v84a : Theory.Target.t
  val v85a : Theory.Target.t
  val v86a : Theory.Target.t
end


(** The family of targets with switchable endiannes.

    The switchable (context-dependent) endianness was introduced
    in [v7] therefore there are no targets of earlier version.
*)
module Bi : sig
  val parent : Theory.Target.t (** the same as [v7]  *)
  val v7 : Theory.Target.t
  val v7fp : Theory.Target.t
  val v7a : Theory.Target.t
  val v7afp : Theory.Target.t
  val v8a : Theory.Target.t
  val v81a : Theory.Target.t
  val v82a : Theory.Target.t
  val v83a : Theory.Target.t
  val v84a : Theory.Target.t
  val v85a : Theory.Target.t
  val v86a : Theory.Target.t
end

val llvm_a32 : Theory.language
val llvm_t32 : Theory.language
val llvm_a64 : Theory.language


(** [load ()] loads the knowledge base rules for the ARM targets.

    This includes parsing the loader output and enabling backward
    compatibility with the old [Arch.t] representation.
*)
val load : unit -> unit
