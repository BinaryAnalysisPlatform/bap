open Core_kernel
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std
open Bap_ir

val program : symtab -> program term
val sub : block -> cfg -> sub term
val blk : cfg -> block -> blk term list
val insn : ?addr:addr -> insn -> blk term list

val insns :
  ?fall:[`Inter of Ir_jmp.dst | `Intra of Ir_jmp.dst ] ->
  ?addr:addr ->
  insn list ->
  blk term list
