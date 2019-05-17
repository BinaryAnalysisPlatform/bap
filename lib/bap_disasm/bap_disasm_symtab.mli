open Bap_core_theory

open Core_kernel
open Bap_types.Std
open Image_internal_std

module Disasm = Bap_disasm_speculative

type block = Bap_disasm_block.t
type edge =  Bap_disasm_block.edge
type cfg = Bap_disasm_rec.Cfg.t

type t [@@deriving compare, sexp_of]
type symtab = t [@@deriving compare, sexp_of]
type fn = string * block * cfg [@@deriving compare, sexp_of]


val build : Disasm.t -> t KB.t
val create : Disasm.t -> t

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


(** {2 Callgraph Interface}

    In parallel to a collection of control flow graphs,
    Symtab contains a callgraph.
*)

(** [insert_call ?implicit symtab callsite callee] remembers a call to a
    function with name [callee] from a callsite, represented by the
    [block].

    If [implicit] is true (defaults to false) then the call is marked
    as an implicit call. An implicit call is a call that is made via
    a fallthrough edge.

    Note, the callee is represented with a string not with an address,
    since it is possible that [find_by_name callee = None], for
    example, when we have a call to an external function out of our
    address space. *)
val insert_call : ?implicit:bool -> t -> block -> string -> t

(** [explicit_callee symtab address] returns a callee which is
    explicitly called from a  block with the given [address]. *)
val explicit_callee : t -> addr -> string option

(** [explicit_callee symtab address] returns a callee which is
    implicitly called from a  block with the given [address]. *)
val implicit_callee : t -> addr -> string option
