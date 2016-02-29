open Bap.Std
open Regular.Std

type t = Arm_types.reg with bin_io, compare, sexp

(** lifts basic register to a ARM one  *)
val create : reg -> t option

include Regular with type t := t
