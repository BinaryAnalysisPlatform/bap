open Bap.Std
open Bap_core_theory

type t

val slot : (_ Theory.Program.Semantics.cls, t) KB.slot

val reify : t -> blk term list
val init : unit -> unit

module Theory : Theory.Core
