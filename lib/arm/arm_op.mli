open Bap.Std
open Regular.Std

type t = Arm_types.op [@@deriving bin_io, compare, sexp]
include Regular.S with type t := t


(** [create op] projects bap generic operand into arm specific.
    Floating point operands are currently ignored.*)
val create : op -> t option
