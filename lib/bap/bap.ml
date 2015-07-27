open Core_kernel.Std

module Std = struct
  type 'a printer = Format.formatter -> 'a -> unit
  include Bap_types.Std
  include Bap_image_std
  include Bap_disasm_std
  include Bap_sema.Std
  module Project = Bap_project
  type project = Project.t
  module Dwarf = Bap_dwarf
  module Elf = Bap_elf
  type elf = Elf.t
  module Signatures = Bap_signatures
  module Byteweight = Bap_byteweight

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

  module Graphlib = struct
    include Bap_graph
    module type Graphs = Bap_graph_regular_intf.S
    include Bap_graph_regular
    module Ir = Bap_ir_graph
  end

end

(* load internal plugins *)
let internal : (module Unit) list = [
  (module Bap_llvm);
  (module Bap_llvm_loader);
  (module Bap_native_loader);
]
