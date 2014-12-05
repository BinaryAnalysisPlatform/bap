module Basic = Bap_disasm_basic

module Insn = struct
  include Basic.Insn
  module Kind = Bap_insn_kind
end

module Op  = Basic.Op
module Reg = Basic.Reg
module Fmm = Basic.Fmm
module Imm = Basic.Imm


type reg = Reg.t with bin_io, compare, sexp
type imm = Imm.t with bin_io, compare, sexp
type fmm = Fmm.t with bin_io, compare, sexp
type (+'a,+'k) insn


(** ARM instruction set  *)
module Arm = struct
  include Bap_disasm_arm
  module Lift = Bap_disasm_arm_lifter
end
