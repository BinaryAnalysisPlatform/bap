open Bap_core_theory
open Bap.Std
open Bap_primus_lisp_types
open Bap_primus_lisp_program


type KB.conflict += Unresolved_definition of string
type KB.conflict += Illtyped_program of Type.error list

val program : (Theory.Source.cls, program) KB.slot
val definition : (Theory.program, Theory.Label.t option) KB.slot
val name : (Theory.program, KB.Name.t option) KB.slot
val args : (Theory.program, unit Theory.Value.t list option) KB.slot
val symbol : (Theory.Value.cls, String.t option) KB.slot
val static : (Theory.Value.cls, Bitvec.t option) KB.slot
val enable : ?stdout:Format.formatter -> unit -> unit

val typed_program : Theory.Unit.t -> program KB.t

val declare :
  ?types:(Theory.Target.t -> Bap_primus_lisp_type.signature) ->
  ?docs:string ->
  ?package:string ->
  string -> unit

module Unit : sig
  val create : ?name:string -> Theory.Target.t -> Theory.Unit.t KB.t
  val is_lisp : Theory.Unit.t -> bool KB.t
  val language : Theory.language
end
