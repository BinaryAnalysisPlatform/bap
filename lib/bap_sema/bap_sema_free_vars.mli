open Bap_types.Std
open Graphlib.Std
open Bap_ir

module Live : sig
  type t
  val compute : ?keep:Var.Set.t -> sub term -> t
  val ins : t -> tid -> Var.Set.t
  val vars : t -> Var.Set.t
  val outs : t -> tid -> Var.Set.t
  val defs : t -> tid -> Var.Set.t
  val uses : t -> tid -> Var.Set.t
  val fold : t -> init:'a -> f:('a -> tid -> Var.Set.t -> 'a) -> 'a
  val blks : t -> var -> Tid.Set.t
  val solution : t -> (tid, Var.Set.t) Solution.t
  val pp : Format.formatter -> t -> unit
end

val compute_liveness : sub term -> (tid, Var.Set.t) Solution.t
val free_vars_of_sub : sub term -> Var.Set.t
