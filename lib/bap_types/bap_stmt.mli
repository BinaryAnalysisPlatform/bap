open Core_kernel
open Bap_core_theory
open Bap_knowledge
open Regular.Std
open Bap_common
open Bap_bil


include Regular.S with type t := stmt

val pp_stmts : Format.formatter -> stmt list -> unit

module Attribute : sig
  type 'a t
  val declare :
    ?package:string ->
    encode:('a -> string) ->
    decode:(string -> 'a) ->
    string ->
    'a t
end


module Stmt : sig
  val move : var -> exp -> stmt
  val jmp : exp -> stmt
  val special : string -> stmt
  val while_ : exp -> stmt list -> stmt
  val if_ : exp -> stmt list -> stmt list -> stmt
  val cpuexn : int -> stmt
  val call : string Attribute.t
  val intrinsic : string Attribute.t
  val encode : 'a Attribute.t -> 'a -> stmt
  val decode : 'a Attribute.t -> stmt -> 'a option
end

module Infix : sig
  val (:=) : var -> exp -> stmt
end

module Stmts_pp : Printable.S with type t = stmt list
module Stmts_data : Data.S with type t = stmt list

val slot : (Theory.Semantics.cls, stmt list) Knowledge.slot
val code : (Theory.program, stmt list) KB.slot
val domain : stmt list Knowledge.domain
val persistent : stmt list Knowledge.persistent
