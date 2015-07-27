open Core_kernel.Std
open Bap_common
open Bap_graph_intf
open Bap_graph
open Bap_bil
open Bap_graph_regular_intf

module Make(Node : Opaque)(Edge : Opaque) : Graph
  with type node = Node.t
   and type Node.label = Node.t
   and type Edge.label = Edge.t


module Char   : S with type node = char
module Int    : S with type node = int
module Word   : S with type node = word
module Type   : S with type node = typ
module Value  : S with type node = Bap_value.t
module String : S with type node = string
module Var    : S with type node = var
module Exp    : S with type node = exp
module Stmt   : S with type node = stmt
module Tid    : S with type node = Bap_ir.tid
