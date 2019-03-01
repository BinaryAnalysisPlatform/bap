open Core_kernel
open Bap_knowledge
open Bap_core_theory_sort

type 'a t
type ord
type ident [@@deriving bin_io, compare, sexp]

val define : 'a sort -> string -> 'a t
val create : 'a sort -> ident -> 'a t

val ident : 'a t -> ident
val name : 'a t -> string
val sort : 'a t -> 'a sort
val is_virtual : 'a t -> bool
val is_mutable : 'a t -> bool
val fresh : 'a sort -> 'a t knowledge
val scoped : 'a sort -> ('a t -> 'b knowledge) -> 'b knowledge

module Ident : Base.Comparable.S with type t = ident
                                  and type comparator_witness = ord
