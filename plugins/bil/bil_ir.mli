open Bap.Std
open Bap_knowledge
open Bap_core_theory

type t

val slot : (Semantics.cls, t) Knowledge.slot

val reify : t -> blk term list
val init : unit -> unit
val pp : Format.formatter -> t -> unit

module Theory : Theory.Core
