open Cmdliner
type t = [`file of string | `stdout] * string * string option with sexp

val t : t Arg.converter

