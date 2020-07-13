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

module Unit : sig
  open Knowledge
  type cls

  type t = cls obj

  val cls : (cls,unit) Class.t

  val for_file : string -> t knowledge

  val for_region : lower:Bitvec.t -> upper:Bitvec.t -> t knowledge

  val path : (cls, string option) slot
  val bias : (cls, Bitvec.t option) slot

  module Target : sig
    val arch : (cls, string option) slot
    val subarch : (cls, string option) slot
    val vendor : (cls, string option) slot
    val system : (cls, string option) slot
    val bits   : (cls, int option) slot
    val abi    : (cls, string option) slot
    val fabi   : (cls, string option) slot
    val cpu    : (cls, string option) slot
    val fpu    : (cls, string option) slot
    val is_little_endian : (cls, bool option) slot
  end

  module Source : sig
    val language : (cls, string option) slot
  end

  module Compiler : sig
    val name : (cls, string option) slot
    val version : (cls, string option) slot
  end

  include Knowledge.Object.S with type t := t
end


module Label : sig
  open Knowledge
  type t = program obj
  val unit : (program, Unit.t option) slot
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
