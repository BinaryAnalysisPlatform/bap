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
  val data : t -> value Sequence.t
end

module type S = sig
  type t with bin_io, compare, sexp
  val pp : Format.formatter -> t -> unit
end

module Tag : sig
  type 'a t = 'a tag
  val to_string : 'a tag -> string
  val register : name:literal -> uuid:literal ->
    (module S with type t = 'a) -> 'a tag
end

module Typeid : Regular with type t = typeid

include Regular with type t := t
