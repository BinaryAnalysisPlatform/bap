open Bap.Std
open Bap_core_theory

open Bap_primus_lisp_types
open Bap_primus_lisp_type
module Def = Bap_primus_lisp_def
module Context = Bap_primus_lisp_context

type t
type program = t
type 'a item

val empty : t
val is_empty : t -> bool
val equal : t -> t -> bool
val merge : t -> t -> t
val add : t -> 'a item -> 'a Def.t -> t
val get : t -> 'a item -> 'a Def.t list
val fold : t -> 'a item -> init:'s ->
  f:(package:string -> 'a Def.t -> 's -> 's) -> 's
val context : t -> Context.t
val sources : t -> Source.t
val package : t -> string
val with_sources : t -> Source.t -> t
val with_context : t -> Context.t -> t
val with_package : t -> string -> t
val reset_package : t -> t
val use_package : t -> ?target:string -> string -> t
val in_package : string -> t -> (t -> 'a) -> 'a


module Items : sig
  val macro : Def.macro item
  val subst : Def.subst item
  val const : Def.const item
  val place : Def.place item
  val func  : Def.func  item
  val meth  : Def.meth  item
  val para  : Def.para item
  val primitive : Def.prim item
  val signal : Def.signal item
end

module Type : sig
  type env
  type error
  val empty : env
  val equal : env -> env -> bool
  val merge : env -> env -> env
  val infer : ?externals:(KB.Name.t * signature) list -> Var.t seq -> program -> env
  val program : env -> program
  val check : Var.t seq -> program -> error list
  val errors : env -> error list
  val pp_error : Format.formatter -> error -> unit
end

val pp : Format.formatter -> t -> unit
val pp_ast : Format.formatter -> ast -> unit
