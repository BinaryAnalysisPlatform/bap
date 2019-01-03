open Core_kernel
open Bap_knowledge
open Bap_core_theory_sort
module Value = Bap_core_theory_value

type 'a t
type ident [@@deriving bin_io, compare, sexp]

val define : 'a sort -> string -> 'a t
val create : 'a sort -> ident -> 'a t

val ident : 'a t -> ident
val name : 'a t -> string
val sort : 'a t -> 'a sort
val is_virtual : 'a t -> bool
val is_mutable : 'a t -> bool
val fresh : 'a sort -> 'a t knowledge
val scoped : 'a sort -> ('a t -> 'b Value.t knowledge) -> 'b Value.t knowledge

module Ident : Identifiable with type t := ident
