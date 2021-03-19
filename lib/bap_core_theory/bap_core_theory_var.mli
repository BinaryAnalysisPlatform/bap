open Core_kernel
open Bap_knowledge
open Bap_core_theory_value


type 'a t
type ord
type ident [@@deriving bin_io, compare, sexp]
type 'a pure = 'a Bap_core_theory_value.t knowledge


val define : 'a sort -> string -> 'a t
val create : 'a sort -> ident -> 'a t
val forget : 'a t -> unit t
val resort : 'a t -> 'b sort -> 'b t

val version : 'a t -> int
val versioned : 'a t -> int -> 'a t

val ident : 'a t -> ident
val name : 'a t -> string
val sort : 'a t -> 'a sort
val is_virtual : 'a t -> bool
val is_mutable : 'a t -> bool
val fresh : 'a sort -> 'a t knowledge
val scoped : 'a sort -> ('a t -> 'b pure) -> 'b pure
val pp : Format.formatter -> 'a t -> unit

module Ident : sig
  type t = ident [@@deriving bin_io, compare, sexp]
  include Stringable.S with type t := t
  include Base.Comparable.S with type t := t
                             and type comparator_witness = ord
end

module Top : sig
  type nonrec t = unit t [@@deriving bin_io, compare, sexp]
  include Base.Comparable.S with type t := t
end
