open Bap_types.Std
open Bap_image
open Image_internal_std

module Basic = Bap_disasm_basic
module Rec = Bap_disasm_rec

module Kind = Bap_insn_kind

module Op  = Basic.Op
module Reg = Basic.Reg
module Fmm = Basic.Fmm
module Imm = Basic.Imm

type reg = Reg.t with bin_io, compare, sexp
type imm = Imm.t with bin_io, compare, sexp
type fmm = Fmm.t with bin_io, compare, sexp
type kind = Kind.t with bin_io, compare, sexp
(** ARM instruction set  *)
module Arm = struct
  include Bap_disasm_arm
  module Lift = Bap_disasm_arm_lifter
end
