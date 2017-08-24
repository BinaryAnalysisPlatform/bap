open Core_kernel.Std

module Std = struct
  type x86_syntax = [`att | `intel] [@@deriving sexp]

  let llvm_version = Bap_llvm_config.version
  let init_disassembler = Bap_llvm_disasm.init
  let init_loader = Bap_llvm_ogre_loader.init
end
