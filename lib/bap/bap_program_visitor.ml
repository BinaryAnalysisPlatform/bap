open Bap_types.Std
open Image_internal_std
open Bap_disasm

type project = {
  arch    : arch;
  program : disasm;
  symbols : string table;
  memory  : mem;
  annots  : tags table;
}

let visitors = ref []
let register v = visitors := v :: !visitors
let registered () = List.rev !visitors
