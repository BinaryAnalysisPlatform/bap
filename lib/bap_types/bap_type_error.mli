open Core_kernel.Std
open Regular.Std
open Bap_common

type t = [
  | `bad_kind of [`mem | `imm]
  | `bad_type of typ * typ
  | `bad_cast
] with bin_io, compare, sexp

val bad_mem : t
val bad_imm : t
val bad_cast : t
val bad_type : exp:typ -> got:typ -> t

type type_error = t with bin_io, compare, sexp

include Regular with type t := t
