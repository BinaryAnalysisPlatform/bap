(** Extends [arch] interface  *)
open Core_kernel.Std
open Bap_common

val of_string : string -> arch option

val addr_size : arch -> addr_size

val endian : arch -> endian

(** [arch] type implements [Idenfifiable]  interface  *)
include Regular with type t := arch
