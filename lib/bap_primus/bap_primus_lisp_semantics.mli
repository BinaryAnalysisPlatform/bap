open Core_kernel
open Bap_core_theory
open Bap.Std
open Bap_primus_lisp_types
open Bap_primus_lisp_program

type value = unit Theory.Value.t
type KB.conflict += Unresolved_definition of string
type KB.conflict += Illtyped_program of Type.error list
type KB.conflict += Failed_primitive of KB.Name.t * string

val program : (Theory.Source.cls, program) KB.slot
val context : (Theory.Unit.cls, Bap_primus_lisp_context.t) KB.slot
val definition : (Theory.program, Theory.Label.t option) KB.slot
val name : (Theory.program, KB.Name.t option) KB.slot
val args : (Theory.program, unit Theory.Value.t list option) KB.slot
val symbol : (Theory.Value.cls, String.t option) KB.slot
val static : (Theory.Value.cls, Bitvec.t option) KB.slot
val enable : ?stdout:Format.formatter -> unit -> unit
val failp : ('a, Format.formatter, unit, 'b KB.t) format4 -> 'a


val typed_program : Theory.Unit.t -> program KB.t

val declare :
  ?types:(Theory.Target.t -> Bap_primus_lisp_type.signature) ->
  ?docs:string ->
  ?package:string ->
  ?body:(Theory.Target.t -> (Theory.Label.t -> Theory.Value.Top.t list -> unit Theory.eff) KB.t) ->
  string ->
  unit



module Unit : sig
  val create : ?name:string -> Theory.Target.t -> Theory.Unit.t KB.t
  val is_lisp : Theory.Unit.t -> bool KB.t
  val language : Theory.language
end


module Value : sig
  type t = unit Theory.Value.t
  val static : Bitvec.t -> t
  val symbol : string -> t
  val custom : (Theory.Value.cls, 'a) KB.slot -> 'a -> t
  val nil : t
end


module Effect : sig
  type t = unit Theory.Effect.t
  val pure : Value.t -> t
  val return : Value.t -> t KB.t
end

val signal :
  ?params:[< `All of Theory.target -> Bap_primus_lisp_types.typ
          | `Gen of
               (Theory.target -> Bap_primus_lisp_types.typ) list *
               (Theory.target -> Bap_primus_lisp_types.typ)
          | `Tuple of (Theory.target -> Bap_primus_lisp_types.typ) list ] ->
  ?docs:string -> (Theory.program, 'p) KB.slot ->
  (Theory.Label.t -> 'p -> Value.t list KB.t) ->
  unit
