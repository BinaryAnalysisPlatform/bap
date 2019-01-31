open Core_kernel
open Bap_types.Std

type 'a t [@@deriving sexp_of]
type mem = Bap_memory.t
type 'a hashable = 'a Hashtbl.Hashable.t

val empty : 'a t
val singleton : mem -> 'a -> 'a t
val add : 'a t -> mem -> 'a -> 'a t Or_error.t
val remove : 'a t -> mem -> 'a t
val change : 'a t -> mem -> f:((mem * 'a) seq -> [
    | `rebind of mem * 'a         (** add new mapping instead  *)
    | `update of ((mem * 'a) -> 'a) (** update all bindings      *)
    | `remove                    (** remove all bindings      *)
    | `ignore])                  (** don't touch anything     *)
  -> 'a t

val length : 'a t -> int
val find : 'a t -> mem -> 'a option
val find_addr : 'a t -> addr -> (mem * 'a) option
val intersections : 'a t -> mem -> (mem * 'a) seq
val fold_intersections : 'a t -> mem -> init:'b -> f:(mem -> 'a -> 'b -> 'b) -> 'b
val has_intersections : 'a t -> mem -> bool
val mem : _ t -> mem -> bool
val next : 'a t -> mem -> (mem * 'a) option
val prev : 'a t -> mem -> (mem * 'a) option
val min : 'a t -> (mem * 'a) option
val max : 'a t -> (mem * 'a) option

type ('a,'m) r
val many : ('a, 'a seq) r
val at_least_one : ('a, 'a * 'a seq) r
val one : ('a, 'a) r
val maybe_one : ('a, 'a option) r
val link : one_to:('b,'r) r -> 'a hashable -> 'a t -> 'b t -> 'a -> 'r
val rev_map : one_to:(mem,'r) r -> 'a hashable -> 'a t -> ('a -> 'r) Or_error.t
type 'a ranged = ?start:mem -> ?until:mem -> 'a

val existsi  : ('a t -> f:(mem -> 'a -> bool) -> bool) ranged
val for_alli : ('a t -> f:(mem -> 'a -> bool) -> bool) ranged
val exists   : ('a t -> f:(      'a -> bool) -> bool) ranged
val for_all  : ('a t -> f:(      'a -> bool) -> bool) ranged

val count    : ('a t -> f:('a -> bool) -> int) ranged
val find_if  : ('a t -> f:('a -> bool) -> 'a option) ranged
val find_map : ('a t -> f:('a -> 'b option) -> 'b option) ranged
val fold  : ('a t -> init:'b -> f:('a -> 'b -> 'b) -> 'b) ranged
val iter  : ('a t -> f:('a -> unit) -> unit) ranged

val find_mapi : ('a t -> f:(mem -> 'a -> 'b option) -> 'b option) ranged
val foldi: ('a t -> init:'b -> f:(mem -> 'a -> 'b -> 'b) -> 'b) ranged
val iteri : ('a t -> f:(mem -> 'a -> unit) -> unit) ranged

val map : ('a t -> f:('a -> 'b) -> 'b t) ranged
val mapi : ('a t -> f:(mem -> 'a -> 'b) -> 'b t) ranged

val filter : ('a t -> f:('a -> bool) -> 'a t) ranged
val filter_map : ('a t -> f:('a -> 'b option) -> 'b t) ranged

val filteri : ('a t -> f:(mem -> 'a -> bool) -> 'a t) ranged
val filter_mapi : ('a t -> f:(mem -> 'a -> 'b option) -> 'b t) ranged
val to_sequence : ('a t -> (mem * 'a) Sequence.t) ranged
val regions : ('a t -> mem seq) ranged
val elements : ('a t -> 'a seq) ranged
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
