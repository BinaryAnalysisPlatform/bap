open Core_kernel.Std

type 'a seq = 'a Sequence.t with sexp

val of_array : 'a array -> 'a seq

val cons : 'a -> 'a seq -> 'a seq

module Export : sig
  val (^::) : 'a -> 'a seq -> 'a seq
end
