open Bap_primus_lisp_types
module Def = Bap_primus_lisp_def
module Context = Bap_primus_lisp_context

type t
type 'a item

val create : Context.t -> t
val add : t -> 'a item -> 'a Def.t -> t
val get : t -> 'a item -> 'a Def.t list
val context : t -> Context.t
val constrain : t -> Context.t -> t

module Items : sig
  val macro : Def.macro item
  val subst : Def.subst item
  val const : Def.const item
  val func  : Def.func  item
  val primitive  : Def.closure item
end
