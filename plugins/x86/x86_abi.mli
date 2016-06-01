open Bap.Std

type abi

val name : abi -> string
val arch : abi -> Arch.x86
val supported : unit -> abi list

(** registers x86 ABIs *)
val setup : ?abi:(Arch.x86 -> abi option) -> unit -> unit
