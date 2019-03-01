open Bap_knowledge

type t

val addr : (t -> unit, Bitvec.t option) Knowledge.slot
val name : (t -> unit, string option) Knowledge.slot
val ivec : (t -> unit, int option) Knowledge.slot
