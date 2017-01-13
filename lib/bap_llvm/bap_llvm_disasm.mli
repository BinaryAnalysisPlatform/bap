open Core_kernel.Std

type x86_syntax = [`att | `intel] [@@deriving sexp]

val init : ?x86_syntax:x86_syntax -> unit -> unit Or_error.t

val version : string
