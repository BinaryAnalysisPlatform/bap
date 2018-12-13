open Bap.Std
open Bap_knowledge
open Bap_core_theory

type t

val t : t domain

val reify : t -> blk term list
val init : unit -> unit
val pp : Format.formatter -> t -> unit

module Theory : Theory.Core
