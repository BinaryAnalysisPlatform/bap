open Core_kernel
open Regular.Std
open Bap_common


type t = [
  | `bad_kind of [`mem | `imm]
  | `bad_type of typ * typ
  | `bad_cast
] [@@deriving bin_io, compare, sexp]

exception T of t [@@deriving sexp_of]


val bad_mem : t
val bad_imm : t
val bad_cast : t
val bad_type : exp:typ -> got:typ -> t

val expect_mem : unit -> 'a
val expect_imm : unit -> 'a
val wrong_cast : unit -> 'a
val expect : typ -> got:typ -> 'a

type type_error = t [@@deriving bin_io, compare, sexp]

include Regular.S with type t := t
