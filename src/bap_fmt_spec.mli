open Bap.Std
open Frontend

type t = [`file of string | `stdout] * string * string option [@@deriving sexp]

val parse : string -> [`Ok of t | `Error of string]
val as_flag : t
val converter : t Config.converter
