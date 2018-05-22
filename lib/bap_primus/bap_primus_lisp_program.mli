open Bap.Std

open Bap_primus_lisp_types
module Def = Bap_primus_lisp_def
module Context = Bap_primus_lisp_context

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
  val primitive  : Def.prim item
end

module Type : sig
  type error
  val check : Var.t seq -> program -> error list
  val pp_error : Format.formatter -> error -> unit
end

val pp : Format.formatter -> t -> unit
