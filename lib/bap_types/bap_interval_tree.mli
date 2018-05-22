open Core_kernel.Std

module type Interval = sig 
  type t [@@deriving compare, sexp_of]
  type point [@@deriving compare, sexp_of]
  val lower : t -> point
  val upper : t -> point
end


module type S = sig
  type 'a t [@@deriving sexp_of]
  type key
  type point

  val empty : 'a t
  val singleton : key -> 'a -> 'a t
  val least : 'a t -> point option
  val greatest : 'a t -> point option
  val min_binding : 'a t -> (key * 'a) option
  val max_binding : 'a t -> (key * 'a) option
  val add : 'a t -> key -> 'a -> 'a t
  val dominators : 'a t -> key -> (key * 'a) Sequence.t
  val intersections : 'a t -> key -> (key * 'a) Sequence.t
  val intersects : 'a t -> key -> bool
  val dominates : 'a t -> key -> bool
  val contains : 'a t -> point -> bool
  val lookup : 'a t -> point -> (key * 'a) Sequence.t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val mapi : 'a t -> f:(key -> 'a -> 'b) -> 'b t
  val filter : 'a t -> f:('a -> bool) -> 'a t
  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
  val filter_mapi : 'a t -> f:(key -> 'a -> 'b option) -> 'b t
  val remove : 'a t -> key -> 'a t
  val remove_intersections : 'a t -> key -> 'a t
  val remove_dominators : 'a t -> key -> 'a t
  val to_sequence : 'a t -> (key * 'a) Sequence.t
  include Container.S1 with type 'a t := 'a t
end 

module Make(Interval : Interval) : S 
  with type key := Interval.t 
   and type point := Interval.point

module type Interval_binable = sig
  type t [@@deriving bin_io, compare, sexp]
  type point [@@deriving bin_io, compare, sexp]
  include Interval with type t := t and type point := point
end

module type S_binable = sig
  type 'a t [@@deriving bin_io, compare, sexp]
  include S with type 'a t := 'a t
end

module Make_binable(Interval : Interval_binable) : S_binable
  with type key := Interval.t
   and type point := Interval.point
