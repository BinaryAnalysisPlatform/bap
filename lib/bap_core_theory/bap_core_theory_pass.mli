open Core_kernel
open Bap_knowledge
open Bap_core_theory_definition

module type trans = functor (_ : Core) -> Core


val register :
  ?desc:string ->
  ?package:string -> string -> (module trans) -> unit

val lookup : Knowledge.Name.t -> (module trans)

val apply : Knowledge.Name.t list -> (module Core) -> (module Core)

module Desugar(CT : Core) : Core
