open Bap_primus_lisp_types
module Context = Bap_primus_lisp_context

type t
type 'a item

val create : Context.t -> t
val add : t -> 'a item -> 'a -> t
val get : t -> 'a item -> 'a list
val context : t -> Context.t
val constrain : t -> Context.t -> t

module Items : sig
  open Bap_primus_lisp_def
  val macro : macro t item
  val subst : subst t item
  val const : const t item
  val func  : func  t item
end
