module Std = struct

  module Lnf = Bap_lnf
  include Bap_ir

  module Ir_lift = Bap_sema_lift

  module Program = struct
    include Ir_program
    let lift = Ir_lift.program
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
  end
end
