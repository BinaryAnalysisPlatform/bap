open Bap_primus_types
open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute

type 'a t
type func
type macro
type subst
type const
type 'a primitive

type attrs = Attribute.set

val name : 'a t -> string
val docs : 'a t -> string
val attributes : 'a t -> attrs
val location : 'a t -> loc

type 'a def = ?docs:string -> ?attrs:attrs -> loc -> string -> 'a

module Func : sig
  val create : (var list -> exp -> func t) def
  val args : func t -> var list
  val body : func t -> exp
end

module Macro : sig
  val create : (string list -> sexp -> macro t) def
  val args : macro t -> string list
  val body : macro t -> sexp
  val bind : macro t -> sexp list -> (int * (string * sexp list) list) option
end

module Const : sig
  val create : (value:string -> macro t) def
  val value : const t -> sexp
end

module Subst : sig
  val create : (sexp list -> subst t) def
  val body : subst -> sexp list
end

module Primitive : sig
  val create : ?docs:string -> string -> (value list -> 'a) -> 'a primitive t
end
