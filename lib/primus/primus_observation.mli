open Core_kernel.Std

type 'a t
type 'a statement
type 'e observations

val provide : ?inspect:('a -> Sexp.t) -> string -> 'a t * 'a statement

val add_observer : 'e observations -> 'a t -> ('a -> 'e) -> 'e observations

val with_observers : 'e observations -> 'a statement -> f:(('a -> 'e) list -> 'b) -> 'b

val empty : 'e observations
