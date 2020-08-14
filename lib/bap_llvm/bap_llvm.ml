open Core_kernel

let strip_version ver =
  if String.length ver <> 5 then ver
  else String.sub ver 0 3


module Std = struct
  type x86_syntax = [`att | `intel] [@@deriving sexp]

  let llvm_version = strip_version Bap_llvm_config.version
  let init_disassembler = Bap_llvm_disasm.init
  let init_loader ?base ?pdb_path () =
    ok_exn @@ Bap_llvm_loader.init ?base ?pdb_path ()
end
