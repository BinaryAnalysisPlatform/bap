open Core_kernel
open Bap_core_theory
open Bap_lisp__types
module Def = Bap_lisp__def
module Context = Bap_lisp__context

type t
type program = t
type 'a item

val empty : t
val add : t -> 'a item -> 'a Def.t -> t
val get : t -> 'a item -> 'a Def.t list
val context : t -> Context.t
val sources : t -> Source.t
val with_sources : t -> Source.t -> t
val with_context : t -> Context.t -> t


module Items : sig
  val macro : Def.macro item
  val subst : Def.subst item
  val const : Def.const item
  val func  : Def.func  item
  val meth  : Def.meth  item
  val para  : Def.para item
  val primitive  : Def.primitive item
end

module Type : sig
  type error
  val check : 'a Theory.Var.t Sequence.t -> program -> error list
  val pp_error : Format.formatter -> error -> unit
end

val pp : Format.formatter -> t -> unit
