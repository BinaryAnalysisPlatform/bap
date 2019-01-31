open Core_kernel
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

(* remembers a call to a function from the given block *)
val add_call_name : t -> block -> string -> t

(* finds if there are any calls from the given block *)
val find_call_name : t -> addr -> string option
