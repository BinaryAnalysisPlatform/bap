open Core_kernel.Std
open Bap.Std

type filename = string
type api = string

module type S = sig
  type t
  val language : string
  val parse : (string -> string option) -> string list -> t Or_error.t
  val mapper : t  -> Term.mapper
end

type t = (module S)

let registry = ref []
let process api = registry := api :: !registry
let processors () = !registry
