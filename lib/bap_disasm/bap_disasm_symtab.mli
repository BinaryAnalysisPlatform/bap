open Core_kernel
open Bap_types.Std
open Image_internal_std

type block = Bap_disasm_block.t
type edge =  Bap_disasm_block.edge
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

(** [add_call symtab block name edge] remembers a call to a function
    [name] from the given block with [edge] *)
val add_call : t -> block -> string -> edge -> t

(** [enum_calls t addr] returns a list of calls from a block with
    the given [addr] *)
val enum_calls : t -> addr -> (string * edge) list
