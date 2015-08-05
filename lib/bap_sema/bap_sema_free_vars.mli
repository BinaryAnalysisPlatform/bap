open Bap_types.Std
open Bap_ir

val free_vars_of_sub : sub term -> Var.Set.t
val bind_args : sub term -> sub term
