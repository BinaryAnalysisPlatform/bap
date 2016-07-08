open Bap.Std
open Frontend

type t = [
  | `Binary
  | `File of string
  | `Memory of arch
] [@@deriving sexp]

exception Unknown_arch of string
exception Unrecognized_source


val converter : t Config.converter
