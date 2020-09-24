(** Declares some of the MIPS targets.

    More targets could be added without modifying this module. It is
    advised to inherit from one of the targets defined here to include
    the newly defined target into the mips family, though it is not
    strictly required.
*)
open Bap_core_theory


(** [parent] it the parent of all MIPS.

    No properties are set. *)
val parent : Theory.Target.t

(** {3 The MIPS 32 family}*)

val mips32bi : Theory.Target.t (** The bi-endian MIPS32 (the parent)  *)
val mips32eb : Theory.Target.t (** The big endian MIPS32  *)
val mips32le : Theory.Target.t (** The little endian MIPS32  *)

(** {3 The MIPS 64 family}*)

val mips64bi : Theory.Target.t (** The bi-endian MIPS64 (the parent)  *)
val mips64eb : Theory.Target.t (** The big endian MIPS64  *)
val mips64le : Theory.Target.t (** The little endian MIPS64  *)


(** [load ()] loads the knowledge base rules for the MIPS targets.

    This includes parsing the loader output and enabling backward
    compatibility with the old [Arch.t] representation.
*)
val load : unit -> unit
