(** BIL high level functions.   *)
open Core_kernel.Std
open Bap_common
open Bap_bil
open Bap_visitor
open Bap_result

val find   : <find : 'a -> 'b option; ..> -> 'a -> 'b option
val exists : <find : 'a -> 'b option; ..> -> 'a -> bool
val iter : unit #bil_visitor -> bil -> unit
val fold : 'a #bil_visitor -> init:'a -> bil -> 'a
val map : #bil_mapper -> bil -> bil

class rewriter : exp -> exp -> bil_mapper
val is_referenced : var -> bil -> bool
val is_assigned : ?strict:bool -> var -> bil -> bool
val prune_unreferenced :
  ?such_that:(var -> bool) ->
  ?physicals:bool ->
  ?virtuals:bool ->
  bil -> bil
val normalize_negatives : bil -> bil

(** [substitute x y p] substitutes each occurrence of expression [x] by
    expression [y] in program [p] *)
val substitute : exp -> exp -> bil -> bil


(** [substitute_var x y p] substitutes all occurences of variable [x]
    by expression [y] *)
val substitute_var : var -> exp -> bil -> bil


val free_vars : bil -> Bap_var.Set.t

(** [fold_consts] evaluate constant expressions.
    Note: this function performs only one step, and has no loops,
    it is supposed to be run using a fixpoint combinator.
*)
val fold_consts : bil -> bil


(** [fixpoint f] applies transformation [f] until fixpoint is
    reached. If the transformation orbit contains non-trivial cycles,
    then the transformation will stop at an arbitrary point of a
    cycle. *)
val fixpoint : (bil -> bil) -> (bil -> bil)


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
  val eval : exp -> value
end

module Stmt : sig
  class state : stmt_state
  class ['a] visitor : ['a] bil_visitor
  class mapper  : bil_mapper
  class ['a] finder : ['a] bil_finder
  val fold : 'a #visitor -> init:'a -> stmt -> 'a
  val iter : unit #visitor -> stmt -> unit
  val find : 'a #finder -> stmt -> 'a option
  val map : #mapper -> bil -> bil
  val exists : unit #finder -> stmt -> bool
  val substitute : exp -> exp -> stmt -> bil
  val is_referenced : var -> stmt -> bool
  val fixpoint : (stmt -> stmt) -> (stmt -> stmt)
  val free_vars : stmt -> Bap_var.Set.t
  val eval : stmt list -> (#Bap_bili.context as 'a) -> 'a

  (** [constant_folder] is a class that implements the [fold_consts]  *)
  class constant_folder : bil_mapper
end

module Trie : sig
  type normalized_bil
  val normalize : ?subst:(exp * exp) list -> bil -> normalized_bil
  module Normalized : Trie with type key = normalized_bil
  include Trie with type key = bil
end
