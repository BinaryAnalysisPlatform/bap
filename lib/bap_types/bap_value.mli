open Core_kernel.Std
open Bap_common

type t with bin_io, compare, sexp
type value = t with bin_io, compare, sexp
type typeid with bin_io, compare, sexp
type 'a tag
type dict with bin_io, compare, sexp
type void
type literal = (void,void,void) format

val create : 'a tag -> 'a -> t
val get : 'a tag -> t -> 'a option
val get_exn : 'a tag -> t -> 'a
val is  : 'a tag -> t -> bool
val tagname : t -> string
val typeid : t -> typeid

module Dict : sig
  type t = dict with bin_io, compare, sexp
  val empty : t
  val is_empty : t -> bool
  val set : t -> 'a tag -> 'a -> t
  val remove : t -> 'a tag -> t
  val mem : t -> 'a tag -> bool
  val find : t -> 'a tag -> 'a option
  val add : t -> 'a tag -> 'a -> [`Ok of t | `Duplicate]
  val change : t -> 'a tag -> ('a option -> 'a option) -> t
  val all_pairs : t -> (typeid * value) Sequence.t
  val data : t -> value Sequence.t
end

module type S = sig
  type t with bin_io, compare, sexp
  val pp : Format.formatter -> t -> unit
end

module Tag : sig
  type 'a t = 'a tag
  val register : name:literal -> uuid:literal ->
    (module S with type t = 'a) -> 'a tag

  val name : 'a tag -> string
  val same : 'a t -> 'b t -> bool
  val same_witness : 'a t -> 'b t -> ('a,'b) Type_equal.t option
  val same_witness_exn : 'a t -> 'b t -> ('a,'b) Type_equal.t
end

module Match : sig
  type 'a t
  val switch : value -> 's t -> 's
  val select : 's t -> value -> 's
  val case : 'a tag -> ('a -> 's) -> 's t -> 's t
  val default : (unit -> 's) -> 's t
end

module Typeid : Regular with type t = typeid

include Regular with type t := t
