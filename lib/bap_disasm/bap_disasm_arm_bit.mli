open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_arm_types



val extend : dest:op -> src:op -> ?src2:op -> sign -> [< `B | `H ] -> rot:op -> op -> stmt list

val bit_field_insert : dest:op -> src:op -> Word.t -> op -> stmt list


val bit_extract : dest:op -> src:op -> sign -> lsb:op -> widthminus1:op -> op -> stmt list
