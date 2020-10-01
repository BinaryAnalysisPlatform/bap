open Core_kernel
open Bap_knowledge

module Effect = Bap_core_theory_effect
module Target = Bap_core_theory_target

type cls
type program = cls
type language
type compiler

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

module Source : sig
  open Knowledge
  type cls
  include Knowledge.Value.S with type t = (cls,unit) Class.t Value.t
  val cls : (cls,unit) Class.t
  val language : (cls,language) slot
  val code : (cls,string) slot
  val file : (cls,string option) slot
end

module Language : Target.Enum.S with type t = language

module Unit : sig
  open Knowledge
  type cls

  type t = cls obj

  val cls : (cls,unit) Class.t

  val for_file : string -> t knowledge

  val for_region : lower:Bitvec.t -> upper:Bitvec.t -> t knowledge

  val path : (cls, string option) slot
  val bias : (cls, Bitvec.t option) slot

  val target : (cls, Target.t) slot
  val source : (cls, Source.t) slot
  val compiler : (cls, compiler option) slot

  include Knowledge.Object.S with type t := t
end

module Compiler : sig
  include Base.Comparable.S with type t = compiler
  include Binable.S with type t := t
  include Pretty_printer.S with type t := t

  val create :
    ?specs:(string * string) list ->
    ?version:string list ->
    ?options:string list ->
    string -> compiler
  val version : compiler -> string list
  val options : compiler -> string list
  val specs : compiler -> string Map.M(String).t
  val to_string : compiler -> string
end

module Label : sig
  open Knowledge
  type t = program obj
  val unit : (program, Unit.t option) slot
  val addr : (program, Bitvec.t option) slot
  val name : (program, string option) slot
  val ivec : (program, Int.t option) slot
  val encoding : (program, language) slot
  val aliases : (program, Set.M(String).t) slot
  val possible_name : (program, string option opinions) slot
  val is_valid : (program, bool option) slot
  val is_subroutine : (program, bool option) slot
  val for_addr : ?package:string -> Bitvec.t -> t knowledge
  val for_name : ?package:string -> string -> t knowledge
  val for_ivec : ?package:string -> int -> t knowledge
  val target : t -> Target.t knowledge

  include Knowledge.Object.S with type t := t
end
