open Bap.Std
open Regular.Std

type t = Arm_types.op [@@deriving bin_io, compare, sexp]
include Regular with type t := t

val create : op -> t option
