open Core_kernel.Std
open Bap_types.Std
open Image_internal_std

type block = Bap_disasm_block.t

type t
type fn

val empty : t
val reconstruct :
  ?name:(addr -> string option) ->
  ?roots:addr list -> block table -> t
val add_symbol : t -> string -> block -> block seq -> t
val remove : t -> fn -> t
val find_by_name  : t -> string -> fn option
val find_by_start : t -> addr -> fn option
val fns_of_addr : t -> addr -> fn list
val fns_of_mem : t -> mem -> fn list
val create_bound : t -> fn -> (addr -> bool) Staged.t
val name_of_fn : fn -> string
val entry_of_fn : fn -> block
val memory_of_fn : t -> fn -> unit memmap
val to_sequence : t -> fn seq
