open Bap_knowledge

module Effect = Bap_core_theory_effect

type cls
type program = cls
type t = cls Knowledge.value
val cls : program Knowledge.cls
module Semantics : sig
  type cls = unit Effect.spec
  type t = cls Knowledge.value
  val cls : cls Knowledge.cls
  val slot : (program, t) Knowledge.slot
  include Knowledge.Value.S with type t := t
end

include Knowledge.Value.S with type t := t

module Label : sig
  type t = program Knowledge.obj
  val addr : (program, Bitvec.t option) Knowledge.slot
  val name : (program, string option) Knowledge.slot
  val ivec : (program, int option) Knowledge.slot
  include Knowledge.Object.S with type t := t
end
