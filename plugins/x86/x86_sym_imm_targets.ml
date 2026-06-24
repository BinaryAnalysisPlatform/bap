open Core_kernel [@@warning "-D"]
open Bap.Std
module Insn = Disasm_expert.Basic.Insn
module IA32SI_L = X86_sym_imm_lifter.IA32
module AMD64SI_L = X86_sym_imm_lifter.AMD64

module IA32SI_D = struct
  module CPU = IA32SI_L.CPU

  let lift _mem _insn = Ok []
end

module AMD64SI_D = struct
  module CPU = AMD64SI_L.CPU

  let lift _mem _insn = Ok []
end

module IA32SI = X86_backend.IA32.Make (IA32SI_D)
module AMD64SI = X86_backend.AMD64.Make (AMD64SI_D)
module IA32SI_M = X86_backend.IA32.Make (IA32SI_L)
module AMD64SI_M = X86_backend.AMD64.Make (AMD64SI_L)
