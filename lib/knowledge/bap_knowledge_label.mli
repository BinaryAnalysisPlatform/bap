type t
val root : t
val succ : t -> t
val to_string : t -> string
val pp : Format.formatter -> t -> unit
include Base.Comparable.S with type t := t
