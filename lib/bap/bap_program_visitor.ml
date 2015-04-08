open Bap_types.Std
open Image_internal_std
open Bap_disasm

type project = {
  arch    : arch;
  argv    : string array;
  program : disasm;
  symbols : string table;
  memory  : mem;
  annots  : (string * string) memmap;
  bil_of_insns : (mem * insn) list -> bil;
}

let visitors = ref []
let register v = visitors := v :: !visitors
let register' v = register (fun p -> v p; p)
let registered () = List.rev !visitors
