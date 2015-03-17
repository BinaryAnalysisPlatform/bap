open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std

type t
type tree
type block

val trees : t -> tree list
val children : tree -> t

val headers : tree -> block list
val bodies : tree -> block list


val create : ?bound:mem -> Block.t -> t
