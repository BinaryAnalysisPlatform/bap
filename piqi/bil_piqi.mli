open Bap.Std

type fmt = [ `json | `pb | `piq | `pib | `xml ]

val bil_of_string : fmt -> string -> bil
val string_of_bil : fmt -> bil -> string

val stmt_of_string : fmt -> string -> stmt
val string_of_stmt : fmt -> stmt -> string

val exp_of_string : fmt -> string -> exp
val string_of_exp : fmt -> exp -> string

val exp_of_piqi : Stmt_piqi.expr -> exp
val var_of_piqi : Stmt_piqi.var -> var
val piqi_of_exp : exp -> Stmt_piqi.expr
val piqi_of_var : var -> Stmt_piqi.var
