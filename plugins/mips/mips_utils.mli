open Bap.Std

(** [mips_fail error_string] - raise a failure with [error_string] *)
val mips_fail : ('a, unit, string, 'b) format4 -> 'a
