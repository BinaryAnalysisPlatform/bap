module Std = struct
  include Bap_graph_intf
  open Bap_graph
  type nonrec 'a tree = 'a tree
  type nonrec 'a frontier = 'a frontier
  type nonrec 'a partition = 'a partition
  type nonrec 'a group = 'a group
  type nonrec 'a path = 'a path
  type nonrec equiv = equiv
  module Group = Group
  module Tree = Tree
  module Frontier = Frontier
  module Partition = Partition
  module Equiv = Equiv
  module Path = Path
  module type Aux = Bap_graph_regular_intf.Aux

  module Graphlib = struct
    include Bap_graph
    module type Graphs = Bap_graph_regular_intf.S
    include Bap_graph_regular
    module Ir = Bap_ir_graph
    module Callgraph = Bap_ir_callgraph
  end
end
