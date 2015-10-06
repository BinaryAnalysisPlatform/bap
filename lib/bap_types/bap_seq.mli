open Core_kernel.Std
open Format

type 'a seq = 'a Sequence.t with sexp

val of_array : 'a array -> 'a seq

val cons : 'a -> 'a seq -> 'a seq

val is_empty : 'a seq -> bool

(** for compatibility with Core kernel < 111.28  *)
val filter : 'a seq -> f:('a -> bool) -> 'a seq
val compare : ('a -> 'b -> int) -> 'a seq -> 'b seq -> int

val max_printer_depth : int ref

val pp : (formatter -> 'a -> unit) -> formatter -> 'a seq -> unit

val pp_bools : formatter -> bool seq -> unit
val pp_chars : formatter -> char seq -> unit
val pp_floats : formatter -> float seq -> unit
val pp_ints : formatter -> int seq -> unit
val pp_strings : formatter -> string seq -> unit

module Export : sig
  val (^::) : 'a -> 'a seq -> 'a seq
end
