
open Core_kernel.Std
open Bap_common

type t with bin_io, compare, sexp
include module type of Uuidm with type t := t
include Regular with type t := t

val of_string: string -> t
