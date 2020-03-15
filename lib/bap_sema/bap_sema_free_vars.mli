open Bap_types.Std
open Graphlib.Std
open Bap_ir

val compute_liveness : sub term -> (tid, Var.Set.t) Solution.t
val free_vars_of_sub : sub term -> Var.Set.t
