type t
val root : t
val succ : t -> t
val pp : Format.formatter -> t -> unit
include Base.Comparable.S with type t := t
