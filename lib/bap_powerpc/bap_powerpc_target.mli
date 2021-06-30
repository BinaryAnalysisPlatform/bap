(** Declares some of the PowerPC targets.

    More targets could be added without modifying this module. It is
    advised to inherit from one of the targets defined here to include
    the newly defined target into the powerpc family, though it is not
    strictly required.
*)
open Bap_core_theory


(** [parent] it the parent of all PowerPC.

    No properties are set. *)
val parent : Theory.Target.t

(** {3 The PowerPC 32 family}*)

val powerpc32bi : Theory.Target.t (** The bi-endian PowerPC32 (the parent)  *)
val powerpc32eb : Theory.Target.t (** The big endian PowerPC32  *)
val powerpc32le : Theory.Target.t (** The little endian PowerPC32  *)

(** {3 The PowerPC 64 family}*)

val powerpc64bi : Theory.Target.t (** The bi-endian PowerPC64 (the parent)  *)
val powerpc64eb : Theory.Target.t (** The big endian PowerPC64  *)
val powerpc64le : Theory.Target.t (** The little endian PowerPC64  *)


(** {2 The PowerPC encodings}  *)

val llvm_powerpc32 : Theory.language
val llvm_powerpc64 : Theory.language

(** [load ()] loads the knowledge base rules for the PowerPC targets.

    This includes parsing the loader output and enabling backward
    compatibility with the old [Arch.t] representation.
*)
val load : ?backend:string -> unit -> unit
