open Bap.Std

val use_ssa : bil -> Exp.Set.t

val def_ssa : bil ->  Exp.Set.t

val use_freevars : bil -> Var.Set.t

val def_freevars : bil -> Var.Set.t
