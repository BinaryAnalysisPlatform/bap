open Core_kernel
open Graphlib.Std
open Bap_disasm_std

module Ssa = Bap_sema_ssa
module Ir_lift = Bap_sema_lift
module Ir_graph = Bap_ir_graph
module FV = Bap_sema_free_vars

module Std = struct
  include Bap_ir

  module Program = struct
    include Ir_program
    let lift = Ir_lift.program
    let to_graph = Bap_ir_callgraph.create
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
    let to_graph = Bap_tid_graph.create
    let ssa = Ssa.sub
    let is_ssa = Ssa.is_transformed
    let free_vars = FV.free_vars_of_sub
  end

  module Taint = Bap_sema_taint

  module Graphs = struct
    module Tid = Bap_tid_graph
    module Ir = Bap_ir_graph
    module Callgraph = Bap_ir_callgraph
    module Cfg = Cfg

    let () =
      let reg name =
        Pretty_printer.register ("Bap.Std.Graphs."^name^".pp") in
      List.iter ~f:reg ["Tid"; "Ir"; "Callgraph"; "Cfg"]
  end

end
