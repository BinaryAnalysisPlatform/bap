open Core_kernel.Std

type 'a seq = 'a Sequence.t with sexp

val of_array : 'a array -> 'a seq

val cons : 'a -> 'a seq -> 'a seq

val is_empty : 'a seq -> bool

(** for compatibility with Core kernel < 111.28  *)
val filter : 'a seq -> f:('a -> bool) -> 'a seq
val compare : ('a -> 'b -> int) -> 'a seq -> 'b seq -> int

module Export : sig
  val (^::) : 'a -> 'a seq -> 'a seq
end
