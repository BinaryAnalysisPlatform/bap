
module Reg = X86_asm_reg

type reg = [Reg.gpr | Reg.segment_base | Reg.segment] [@@deriving sexp_poly]
