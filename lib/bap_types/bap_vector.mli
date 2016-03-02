open Core_kernel.Std
type 'a t [@@deriving bin_io, compare, sexp]
val create : ?capacity:int -> 'a -> 'a t
val append : 'a t -> 'a -> unit
val nth : 'a t -> int -> 'a option
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val map_to_array : 'a t -> f:('a -> 'b) -> 'b array
val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option
val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b) -> 'b

val index : ?equal:('a -> 'a -> bool) -> 'a t -> 'a -> int option
val index_exn : ?equal:('a -> 'a -> bool) -> 'a t -> 'a -> int
val index_with : ?equal:('a -> 'a -> bool) -> default:int -> 'a t -> 'a -> int

include Container.S1 with type 'a t := 'a t
