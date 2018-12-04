open Bap.Std
open Bap_knowledge

type t

val t : t domain

val reify : t -> blk term list
val init : unit -> unit
val pp : Format.formatter -> t -> unit
