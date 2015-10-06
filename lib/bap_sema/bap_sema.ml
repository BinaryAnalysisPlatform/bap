open Graphlib.Std

module Ssa = Bap_sema_ssa
module Ir_lift = Bap_sema_lift
module Ir_graph = Graphlib.Ir
module FV = Bap_sema_free_vars
module ABI = Bap_sema_abi

module Std = struct
  include Bap_ir



  module Program = struct
    include Ir_program
    let lift = Ir_lift.program
    let to_graph = Graphlib.Callgraph.create
  end

  module Arg = Ir_arg
  module Phi = Ir_phi
  module Jmp = Ir_jmp
  module Def = Ir_def
  module Blk = struct
    include Ir_blk
    let lift = Ir_lift.blk
    let from_insn = Ir_lift.insn
  end
  module Sub = struct
    include Ir_sub
    let lift = Ir_lift.sub
    let of_cfg = Ir_graph.to_sub
    let to_cfg = Ir_graph.of_sub
    let to_graph = Ir_graph.create_tid_graph
    let ssa = Ssa.sub
    let is_ssa = Ssa.is_transformed
    let free_vars = FV.free_vars_of_sub
    let infer_args = ABI.infer_args
  end

end

let () = ABI.register ()
