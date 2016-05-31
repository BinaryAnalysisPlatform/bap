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
type id = t Doubly_linked.Elt.t

let registry = Doubly_linked.create ()
let process api : id = Doubly_linked.insert_last registry api
let retract id = Doubly_linked.remove registry id
let processors () = Doubly_linked.to_list registry
