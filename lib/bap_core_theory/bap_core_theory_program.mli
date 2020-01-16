open Core_kernel
open Bap_knowledge

module Effect = Bap_core_theory_effect

type cls
type program = cls
type t = (program,unit) Knowledge.cls Knowledge.value
val cls : (program,unit) Knowledge.cls
module Semantics : sig
  type cls = Effect.cls
  type t = unit Effect.t
  val cls : (cls, unit Effect.sort) Knowledge.cls
  val slot : (program, t) Knowledge.slot
  include Knowledge.Value.S with type t := t
end

include Knowledge.Value.S with type t := t

module Label : sig
  open Knowledge
  type t = program obj
  val path : (program, string option) slot
  val addr : (program, Bitvec.t option) slot
  val name : (program, string option) slot
  val ivec : (program, Int.t option) slot
  val aliases : (program, Set.M(String).t) slot
  val possible_name : (program, string option opinions) slot
  val is_valid : (program, bool option) slot
  val is_subroutine : (program, bool option) slot
  val for_addr : ?package:string -> Bitvec.t -> t knowledge
  val for_name : ?package:string -> string -> t knowledge
  val for_ivec : ?package:string -> int -> t knowledge

  include Knowledge.Object.S with type t := t
end
