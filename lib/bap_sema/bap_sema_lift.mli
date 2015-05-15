open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std
open Bap_ir


val program : addr list -> block table -> program term
val sub : block -> sub term
val blk : block -> blk term list
