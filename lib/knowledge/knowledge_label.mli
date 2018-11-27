open Core_kernel

type t
val root : t
val succ : t -> t
val pp : Format.formatter -> t -> unit
include Identifiable.S with type t := t
