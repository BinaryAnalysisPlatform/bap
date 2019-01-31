open Core_kernel
open Regular.Std
open Graphlib_intf
open Graphlib_graph
open Graphlib_regular_intf

module Make(Node : Opaque.S)(Label : T) : Graph
  with type node = Node.t
   and type Node.label = Node.t
   and type Edge.label = Label.t

module Labeled(Node : Opaque.S)(NL : T)(EL : T) : Graph
  with type node = (Node.t, NL.t) labeled
   and type Node.label = (Node.t, NL.t) labeled
   and type Edge.label = EL.t

module Aux(M : sig
    include Pretty_printer.S
    val module_name : string
  end) : Aux with type node := M.t
