(** Extends [arch] interface  *)
open Core_kernel.Std
open Bap_common

(** [arch] type implements [Idenfifiable]  interface  *)
include Identifiable with type t := arch
