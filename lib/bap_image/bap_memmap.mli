open Bap_core_theory
open Core_kernel
open Bap_types.Std

type mem = Bap_memory.t

type 'a t [@@deriving sexp_of]

val empty : 'a t
val singleton : mem -> 'a -> 'a t
val min_addr : 'a t -> addr option
val max_addr : 'a t -> addr option
val min_binding : 'a t -> (mem * 'a) option
val max_binding : 'a t -> (mem * 'a) option
val add : 'a t -> mem -> 'a -> 'a t
val dominators : 'a t -> mem -> (mem * 'a) seq
val intersections : 'a t -> mem -> (mem * 'a) seq
val intersects : 'a t -> mem -> bool
val dominates : 'a t -> mem -> bool
val contains : 'a t -> addr -> bool
val lookup : 'a t -> addr -> (mem * 'a) seq
val map : 'a t -> f:('a -> 'b) -> 'b t
val mapi : 'a t -> f:(mem -> 'a -> 'b) -> 'b t
val filter : 'a t -> f:('a -> bool) -> 'a t
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
val filter_mapi : 'a t -> f:(mem -> 'a -> 'b option) -> 'b t
val remove : 'a t -> mem -> 'a t
val remove_intersections : 'a t -> mem -> 'a t
val remove_dominators : 'a t -> mem -> 'a t
val to_sequence : 'a t -> (mem * 'a) Sequence.t
include Container.S1 with type 'a t := 'a t
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val domain : value t KB.domain
