(** Declarations of some x86 targets.  *)

open Bap_core_theory

(** The parent of all x86 targets.

    When a new target is declared it is advised to use any target from
    this module as parent so that the newly declared target will be
    included into the x86 Targets family.  The [parent] target is pure
    abstract and doesn't have any propreties set.  *)
val parent : Theory.Target.t

val i86 : Theory.Target.t
val i186 : Theory.Target.t
val i286 : Theory.Target.t
val i386 : Theory.Target.t
val i486 : Theory.Target.t
val i586 : Theory.Target.t
val i686 : Theory.Target.t
val amd64 : Theory.Target.t


(** The list of x86 abis.

    @since 2.5.0 *)
module Abi : sig
  val cdecl : Theory.abi
  val pascal : Theory.abi
  val fortran : Theory.abi
  val fastcall : Theory.abi
  val stdcall : Theory.abi
  val thiscall : Theory.abi
  val vectorcall : Theory.abi
  val watcom : Theory.abi
  val ms : Theory.abi
  val sysv : Theory.abi
end

(** [load ()] loads the knowledge base rules for the x86 targets.

    This includes parsing the loader output and enabling backward
    compatibility with the old [Arch.t] representation.

    @param abi overrides the automatic selection of abi and use the
    specified one.

    @since 2.4.0 accepts the backend parameter.
    @since 2.5.0 accepts the abi parameter.*)
val load : ?abi:Theory.abi -> ?backend:string -> unit -> unit
