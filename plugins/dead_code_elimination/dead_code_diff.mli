open Core_kernel
open Bap.Std
open Regular.Std

type t

val diff_of_sub : sub term -> sub term -> t

val apply : sub term -> t -> sub term

include Data.S with type t := t
