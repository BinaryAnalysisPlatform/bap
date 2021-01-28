open Bap_core_theory
open Bap.Std
open Bap_primus_lisp_types
open Bap_primus_lisp_program


type primitive

module Primitive : sig
  type t = primitive
  val name : t -> string
  val args : t -> unit Theory.Value.t list
end

val program : (Theory.Source.cls, program) KB.slot
val primitive : (Theory.program, primitive option) KB.slot
val symbol : (Theory.Value.cls, String.t option) KB.slot
val static : (Theory.Value.cls, Bitvec.t option) KB.slot


module Unit : sig
  val create : ?name:string -> Theory.Target.t -> Theory.Unit.t KB.t
  val is_lisp : Theory.Unit.t -> bool KB.t
  val language : Theory.language
end
