open Regular.Std
open Graphlib_intf
open Graphlib_graph

module type Aux = sig
  type node
  module Tree : Printable.S with type t = node tree
  module Frontier : Printable.S with type t = node frontier
  module Path : Printable.S with type t = node path
  module Partition : Printable.S with type t = node partition
  module Group : Printable.S with type t = node group
end


module type S = sig
  type node
  module Bool : Graph with type node = node
                       and type Node.label = node
                       and type Edge.label = bool

  module Unit : Graph with type node = node
                       and type Node.label = node
                       and type Edge.label = unit


  module Int : Graph with type node = node
                      and type Node.label = node
                      and type Edge.label = int

  module String : Graph with type node = node
                         and type Node.label = node
                         and type Edge.label = string
  include Aux with type node := node
end
