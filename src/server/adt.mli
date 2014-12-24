open Core_kernel.Std
open Bap.Std

val strings_of_bil   : stmt list -> string list
val strings_of_ops   : Disasm.Basic.op list -> string list
val strings_of_kinds : Disasm.Basic.kind list -> string list
