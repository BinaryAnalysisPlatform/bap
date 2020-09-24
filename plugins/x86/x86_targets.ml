open Core_kernel
open Bap.Std

module Insn = Disasm_expert.Basic.Insn

module IA32L = X86_lifter.IA32
module AMD64L = X86_lifter.AMD64

module IA32D = struct
  module CPU = IA32L.CPU
  let lift _mem insn =
    Or_error.error "unimplemented" insn Insn.sexp_of_t
end

module AMD64D = struct
  module CPU = AMD64L.CPU
  let lift _mem insn =
    Or_error.error "unimplemented" insn Insn.sexp_of_t
end

module IA32 = X86_backend.IA32.Make(IA32D)
module AMD64 = X86_backend.AMD64.Make(AMD64D)

module IA32M = X86_backend.IA32.Make(IA32L)
module AMD64M = X86_backend.AMD64.Make(AMD64L)
