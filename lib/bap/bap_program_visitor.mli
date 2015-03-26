open Bap_types.Std
open Image_internal_std
open Bap_disasm

type project = {
  arch    : arch;
  program : disasm;
  symbols : string table;
  memory  : mem;
  annots  : (string * string) memmap;
  bil_of_insns : (mem * insn) list -> bil;
}

val register : (project -> project) -> unit
val register': (project -> unit) -> unit
val registered : unit -> (project -> project) list
