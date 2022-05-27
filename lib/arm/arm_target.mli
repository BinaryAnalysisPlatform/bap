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

(** A role for registers available in the thumb mode.  *)
val thumb : Theory.role

(** The family of little endian targets.

    Each version is the parent to the following version, with [parent]
    being the the earliest version.*)
module LE : sig
  val parent : Theory.Target.t

  (** a generic 32-bit target that accomodates all 32-bit targets.

      @since 2.5.0  *)
  val aarch32 : Theory.Target.t

  (** a generic 64-bit target that accomodates all 64-bit targets.

      @since 2.5.0 *)
  val aarch64 : Theory.Target.t

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
  val v7fp : Theory.Target.t
  val v7a : Theory.Target.t
  val v7afp : Theory.Target.t
  val v8a : Theory.Target.t
  val v8a32 : Theory.Target.t
  val v8m32 : Theory.Target.t
  val v8r32 : Theory.Target.t
  val v81a : Theory.Target.t
  val v82a : Theory.Target.t
  val v83a : Theory.Target.t
  val v84a : Theory.Target.t
  val v85a : Theory.Target.t
  val v86a : Theory.Target.t
  val v9a : Theory.Target.t
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
  val v8a32 : Theory.Target.t
  val v8m32 : Theory.Target.t
  val v8r32 : Theory.Target.t
  val v81a : Theory.Target.t
  val v82a : Theory.Target.t
  val v83a : Theory.Target.t
  val v84a : Theory.Target.t
  val v85a : Theory.Target.t
  val v86a : Theory.Target.t
  val v9a : Theory.Target.t
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
  val v8a32 : Theory.Target.t
  val v8m32 : Theory.Target.t
  val v8r32 : Theory.Target.t
  val v81a : Theory.Target.t
  val v82a : Theory.Target.t
  val v83a : Theory.Target.t
  val v84a : Theory.Target.t
  val v85a : Theory.Target.t
  val v86a : Theory.Target.t
  val v9a : Theory.Target.t
end

val llvm_a32 : Theory.language
val llvm_t32 : Theory.language
val llvm_a64 : Theory.language


(** [load ()] loads the knowledge base rules for the ARM targets.

    This includes parsing the loader output and enabling backward
    compatibility with the old [Arch.t] representation.

    @param interworking if set disables/enables the interworking
    mode (switching between arm and thumb modes). If not set, then
    the presence of interworking is detected using heurisitics. Right
    now if the heuristic looks into the symbol table and if there is
    a symbol there with an odd address (which is used to indicate
    thumb encoding) then interworking is enabled.

    @param features is the backend-specific list of features. The
    syntax is vastly dependent on the backend. For llvm, in
    particular, the features are translated to the disassembler
    attributes. If the feature doesn't start with [+] or [-] then it
    is assumed that the feature is enabled and [+] is prepended.
*)
val load : ?features:string list -> ?interworking:bool -> ?backend:string -> unit -> unit
