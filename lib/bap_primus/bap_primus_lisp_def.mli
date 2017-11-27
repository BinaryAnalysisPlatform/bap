open Bap_primus_types
open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute

module type Code = functor(Machine : Machine) -> sig
  val run : value list -> value Machine.t
end

type 'a spec
type 'a t = 'a spec indexed
type func
type macro
type subst
type const
type code = (module Code)
type 'a primitive

type attrs = Attribute.set

val name : 'a t -> string
val docs : 'a t -> string
val attributes : 'a t -> attrs

type 'a def = ?docs:string -> ?attrs:attrs -> string -> 'a

module Func : sig
  val create : (var list -> ast -> tree -> func t) def
  val args : func t -> var list
  val body : func t -> ast
end

module Macro : sig
  val create : (string list -> tree -> tree -> macro t) def
  val args : macro t -> string list
  val body : macro t -> tree
  val bind : macro t -> tree list -> (int * (string * tree list) list) option

  (** [apply m bs] returns the body of [m] where any occurence of a
      variable [x] is substituted with [y] if [x,[y]] is in the list
      of bindings [bs].

      The identity of the returned tree is the same as the identity of
      the macro body.*)
  val apply : macro t -> (string * tree list) list -> tree
end

module Const : sig
  val create : (value:string -> tree -> const t) def
  val value : const t -> tree
end

module Subst : sig
  val create : (tree list -> tree -> subst t) def
  val body : subst t -> tree list
end

module Primitive : sig
  type nonrec 'a t = 'a primitive t
  val create : ?docs:string -> string -> (value list -> 'a) -> 'a t
  val body : 'a t -> (value list -> 'a)
end

module Code : sig
  val of_primitive : 'a Primitive.t -> code -> code t
  val body : code t -> code
end

module type Primitives = functor (Machine : Machine) ->  sig
  val defs : unit -> value Machine.t Primitive.t list
end

type primitives = (module Primitives)
