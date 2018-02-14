open Cmdliner
type t = [`file of string | `stdout] * string * string option [@@deriving sexp]

val t : t Arg.converter

val parse : string -> [`Ok of t | `Error of string]
