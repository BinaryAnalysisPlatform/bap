open Bap_common
open Bap_graph_intf
open Bap_graph
open Bap_bil



module type S = sig
  type node
  module Bool : Graph with type node = node
                       and type Node.label = node
                       and type Edge.label = bool

  module Unit : Graph with type node = node
                       and type Node.label = node
                       and type Edge.label = unit

  module Value : Graph with type node = node
                        and type Node.label = node
                        and type Edge.label = Bap_value.t

  module Word : Graph with type node = node
                       and type Node.label = node
                       and type Edge.label = word

  module Int : Graph with type node = node
                      and type Node.label = node
                      and type Edge.label = int

  module String : Graph with type node = node
                         and type Node.label = node
                         and type Edge.label = string

  module Exp : Graph with type node = node
                      and type Node.label = node
                      and type Edge.label = exp


  module Stmt : Graph with type node = node
                       and type Node.label = node
                       and type Edge.label = stmt


  module Var : Graph with type node = node
                      and type Node.label = node
                      and type Edge.label = var


  module Tid : Graph with type node = node
                      and type Node.label = node
                      and type Edge.label = Bap_ir.tid

  module Type : Graph with type node = node
                       and type Node.label = node
                       and type Edge.label = typ

  module Tree : Printable with type t = node tree
  module Frontier : Printable with type t = node frontier
  module Path : Printable with type t = node path
  module Partition : Printable with type t = node partition
  module Group : Printable with type t = node group
end
