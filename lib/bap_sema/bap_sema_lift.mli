open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std
open Bap_ir


val program : symtab -> program term
val sub : ?bound:(addr -> bool) -> block -> sub term
val blk : block -> blk term list
val insn : insn -> blk term list
