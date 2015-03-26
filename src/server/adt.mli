open Core_kernel.Std
open Bap.Std
open Disasm

val strings_of_bil   : stmt list -> string list
val strings_of_ops   : Basic.op list -> string list
val strings_of_kinds : Basic.kind list -> string list
val strings_of_preds : Basic.pred list -> string list
val string_of_arm    : ARM.Insn.t -> ARM.Op.t list -> string
val string_of_endian : endian -> string

module Parse : sig
  type 'a t = string -> 'a Or_error.t
  val word : word t
  val kind : Basic.kind t
  val pred : Basic.pred t
  val endian : endian t
end
