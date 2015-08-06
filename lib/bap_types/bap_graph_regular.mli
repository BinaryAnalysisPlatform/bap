open Core_kernel.Std
open Bap_common
open Bap_graph_intf
open Bap_graph
open Bap_bil
open Bap_graph_regular_intf

module Make(Node : Opaque)(Label : T) : Graph
  with type node = Node.t
   and type Node.label = Node.t
   and type Edge.label = Label.t

module Labeled(Node : Opaque)(NL : T)(EL : T) : Graph
  with type node = (Node.t, NL.t) labeled
   and type Node.label = (Node.t, NL.t) labeled
   and type Edge.label = EL.t

module Aux(M : sig
    include Pretty_printer.S
    val module_name : string
  end) : Aux with type node := M.t

module Int    : S with type node = int
module Word   : S with type node = word
module Value  : S with type node = Bap_value.t
module String : S with type node = string
module Var    : S with type node = var
module Exp    : S with type node = exp
module Stmt   : S with type node = stmt
module Tid    : S with type node = Bap_ir.tid
