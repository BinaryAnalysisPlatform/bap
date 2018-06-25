open Core_kernel.Std
open Bap.Std

include module type of X86_asm_reg_types

(** [width reg_type] returns the size of the given [reg_type] *)
val width : [gpr | ip] -> size

(** [bitwidth reg_type] returns the width in bits of the
    given [reg_type] *)
val bitwidth : [gpr | ip] -> int

(** [decode reg] decodes the given [reg] provided by the
    disassembler as an x86 register if that is feasible. *)
val decode : reg -> t option
