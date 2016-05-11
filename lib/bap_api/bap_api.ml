open Core_kernel.Std
open Bap.Std


module type S = sig
  type t
  val language : string
  val parse : (string -> string option) -> string list -> t Or_error.t
  val mapper : t  -> Term.mapper
end

type t = (module S)


let registry : t list ref = ref []
let process api = registry := api :: !registry
let processors () = !registry
