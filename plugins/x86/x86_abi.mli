open Bap.Std

type abi

val name : abi -> string
val supported : unit -> abi list

(** registers x86 ABIs *)
val setup : ?abi:abi -> unit -> unit
