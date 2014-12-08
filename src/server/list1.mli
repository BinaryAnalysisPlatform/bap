open Core_kernel.Std

type +'a t

include Container.S1 with type 'a t := 'a t
include Monad.S with type 'a t := 'a t

val create : 'a -> 'a list -> 'a t
val singleton : 'a -> 'a t
val of_list : 'a list -> 'a t option

val hd : 'a t -> 'a
val tl : 'a t -> 'a t
val last : 'a t -> 'a
val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a
