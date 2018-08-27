open Core_kernel.Std
open Bap_types.Std
open Image_internal_std

type block = Bap_disasm_block.t
type cfg = Bap_disasm_rec.Cfg.t

type t [@@deriving compare, sexp_of]
type symtab = t [@@deriving compare, sexp_of]
type fn = string * block * cfg [@@deriving compare, sexp_of]


val empty : t
val add_symbol : t -> fn -> t
val remove : t -> fn -> t
val find_by_name  : t -> string -> fn option
val find_by_start : t -> addr -> fn option
val owners : t -> addr -> fn list
val dominators : t -> mem -> fn list
val intersecting : t -> mem -> fn list
val to_sequence : t -> fn seq
val span : fn -> unit memmap

val add_callee : t -> addr -> string -> t
val find_callee : t -> addr -> string option
