(** BIL high level functions.   *)
open Core_kernel.Std
open Bap_common
open Bap_bil
open Bap_visitor
open Bap_result

class rewriter : exp -> exp -> bil_mapper

val find   : <find : 'a -> 'b option; ..> -> 'a -> 'b option
val exists : <find : 'a -> 'b option; ..> -> 'a -> bool
val iter : unit #bil_visitor -> bil -> unit
val fold : 'a #bil_visitor -> init:'a -> bil -> 'a
val map : #bil_mapper -> bil -> bil

val is_referenced : var -> bil -> bool
val is_assigned : ?strict:bool -> var -> bil -> bool
val prune_unreferenced :
  ?such_that:(var -> bool) ->
  ?physicals:bool ->
  ?virtuals:bool ->
  bil -> bil

val normalize : ?keep_ites:bool -> ?normalize_exp:bool -> bil -> bil
val normalize_negatives : bil -> bil
val substitute : exp -> exp -> bil -> bil
val substitute_var : var -> exp -> bil -> bil
val free_vars : bil -> Bap_var.Set.t
val fold_consts : bil -> bil
val fixpoint : (bil -> bil) -> (bil -> bil)


module Apply : sig
  val binop : binop -> word -> word -> word
  val unop : unop -> word -> word
  val cast : cast -> int -> word -> word
end

module Eff : sig
  type t

  val none : t
  val read : t
  val load : t
  val store : t
  val raise : t

  val reads : t -> bool
  val loads : t -> bool
  val stores : t -> bool
  val raises : t -> bool

  val has_effects : t -> bool
  val has_coeffects :  t -> bool
  val idempotent : t -> bool

  val compute : exp -> t
end

module Simpl : sig
  val bil  : ?ignore:Eff.t list -> stmt list -> stmt list
  val stmt : ?ignore:Eff.t list -> stmt -> stmt list
  val exp  : ?ignore:Eff.t list -> exp -> exp
end


module Exp : sig
  class state : exp_state
  class ['a] visitor : ['a] exp_visitor
  class mapper  : exp_mapper
  class ['a] finder : ['a] exp_finder

  val fold : 'a #visitor -> init:'a -> exp -> 'a
  val iter : unit #visitor -> exp -> unit
  val find : 'a #finder -> exp -> 'a option
  val map  : #mapper -> exp -> exp
  val exists : unit #finder -> exp -> bool
  val substitute : exp -> exp -> exp -> exp
  val is_referenced : var -> exp -> bool
  val normalize_negatives : exp -> exp
  val fold_consts : exp -> exp
  val fixpoint : (exp -> exp) -> (exp -> exp)
  val free_vars : exp -> Bap_var.Set.t
  val normalize : exp -> exp
end

module Stmt : sig
  class state : stmt_state
  class ['a] visitor : ['a] bil_visitor
  class mapper  : bil_mapper
  class ['a] finder : ['a] bil_finder
  class constant_folder : mapper
  val fold : 'a #visitor -> init:'a -> stmt -> 'a
  val iter : unit #visitor -> stmt -> unit
  val find : 'a #finder -> stmt -> 'a option
  val map : #mapper -> bil -> bil
  val exists : unit #finder -> stmt -> bool
  val substitute : exp -> exp -> stmt -> bil
  val is_referenced : var -> stmt -> bool
  val fixpoint : (stmt -> stmt) -> (stmt -> stmt)
  val free_vars : stmt -> Bap_var.Set.t
  val normalize : ?keep_ites:bool -> ?normalize_exp:bool
    -> stmt list -> stmt list
end


module Trie : sig
  type normalized_bil
  val normalize : ?subst:(exp * exp) list -> bil -> normalized_bil
  module Normalized : Trie with type key = normalized_bil
  include Trie with type key = bil
end
