(** Declarations of some x86 targets.  *)

open Bap_core_theory

(** The parent of all x86 targets.

    When a new target is declared it is advised to use this target as
    parent so that the newly declared target will be included into the
    x86 Targets family.
    The [parent] target is pure abstract and doesn't have any
    propreties set.
*)
val parent : Theory.Target.t
val i86 : Theory.Target.t
val i186 : Theory.Target.t
val i286 : Theory.Target.t
val i386 : Theory.Target.t
val i486 : Theory.Target.t
val i586 : Theory.Target.t
val i686 : Theory.Target.t
val amd64 : Theory.Target.t

(** [load ()] loads the knowledge base rules for the x86 targets.

    This includes parsing the loader output and enabling backward
    compatibility with the old [Arch.t] representation.
*)
val load : ?backend:string -> unit -> unit
