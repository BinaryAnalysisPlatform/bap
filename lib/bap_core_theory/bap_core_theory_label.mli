open Bap_knowledge

module Effect = Bap_core_theory_effect
type t = unit Effect.spec Knowledge.Object.t

val addr : (unit Effect.spec, Bitvec.t option) Knowledge.slot
val name : (unit Effect.spec, string option) Knowledge.slot
val ivec : (unit Effect.spec, int option) Knowledge.slot

include Knowledge.Object.S with type t := t
