open Core_kernel
open Bap_core_theory
open Regular.Std
open Bap_types.Std
open Bap_disasm_types
open Bap_ir

type t = Theory.Semantics.t [@@deriving bin_io, compare, sexp]
type op = Op.t [@@deriving bin_io, compare, sexp]

val empty : t

val of_basic : ?bil:bil -> Basic.full_insn -> t

val name : t -> string
val asm  : t -> string
val bil  : t -> bil
val ops  : t -> op array

type must = Must
type may = May
type 'a property

val new_property : 'a -> string -> 'a property

val jump                : must property
val conditional         : must property
val indirect            : must property
val call                : must property
val return              : must property
val barrier             : must property
val affect_control_flow : may  property
val load                : may  property
val store               : may  property

val is  : must property -> t -> bool
val may : may  property -> t -> bool
val must    : must property -> t -> t
val mustn't : must property -> t -> t
val should    : may  property -> t -> t
val shouldn't : may  property -> t -> t


module Slot : sig
  type 'a t = (Theory.Effect.cls, 'a) KB.slot
  val name : string t
  val asm :  string t
  val ops :  op array option t
  val delay : int option t
  val dests : Set.M(Theory.Label).t option t
end


val pp_adt : Format.formatter -> t -> unit
module Trie : sig
  type key
  val key_of_insns : t list -> key
  module Normalized : Trie with type key = key
  include Trie with type key := key
end
include Regular.S with type t := t
