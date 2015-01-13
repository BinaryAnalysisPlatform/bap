(** Extends [arch] interface  *)
open Core_kernel.Std
open Bap_common

val of_string : string -> arch option

(** [arch] type implements [Idenfifiable]  interface  *)
include Regular with type t := arch
