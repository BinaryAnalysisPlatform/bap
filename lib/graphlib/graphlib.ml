module Std = struct
  include Graphlib_intf
  open Graphlib_graph
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
  module Solution = Fixpoint
  module Graphlib = struct
    include Graphlib_graph
    module type Aux = Graphlib_regular_intf.Aux
    module type Graphs = Graphlib_regular_intf.S
    include Graphlib_regular
    include Graphlib_pp
  end
end
