open Bap.Std

type t = [
  | `Binary
  | `File of string
  | `Memory of arch
] with sexp

exception Unknown_arch of string
exception Unrecognized_source


val t : t Cmdliner.Arg.converter
