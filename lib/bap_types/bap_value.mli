open Bap_core_theory

open Core_kernel
open Regular.Std

type t [@@deriving bin_io, compare, sexp]
type value = t [@@deriving bin_io, compare, sexp]
type typeid [@@deriving bin_io, compare, sexp]
type 'a tag
type dict [@@deriving bin_io, compare, sexp]
type void
type literal = (void,void,void) format

val create : 'a tag -> 'a -> t
val get : 'a tag -> t -> 'a option
val get_exn : 'a tag -> t -> 'a
val is  : 'a tag -> t -> bool
val tagname : t -> string
val typeid : t -> typeid

module Dict : sig
  type t = dict [@@deriving bin_io, compare, sexp]
  val empty : t
  val is_empty : t -> bool
  val set : t -> 'a tag -> 'a -> t
  val remove : t -> 'a tag -> t
  val mem : t -> 'a tag -> bool
  val find : t -> 'a tag -> 'a option
  val add : t -> 'a tag -> 'a -> [`Ok of t | `Duplicate]
  val change : t -> 'a tag -> ('a option -> 'a option) -> t
  val to_sequence : t -> (typeid * value) Sequence.t
  val data : t -> value Sequence.t
  val filter : t -> f:(value -> bool) -> t
end

module type S = sig
  type t [@@deriving bin_io, compare, sexp]
  val pp : Format.formatter -> t -> unit
end

module Tag : sig
  type 'a t = 'a tag
  val register :
    ?public:bool -> ?desc:string ->
    ?package:string -> name:string -> uuid:string ->
    (module S with type t = 'a) -> 'a tag

  val register_slot : ?uuid:string -> (Theory.program,'a option) KB.slot -> (module S with type t = 'a) -> 'a tag

  val slot : 'a tag -> (Theory.program, 'a option) KB.slot


  val name : 'a tag -> string
  val typeid  : 'a tag -> typeid
  val key : 'a tag -> 'a Type_equal.Id.t
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

module Typeid : Identifiable.S with type t = typeid

include Regular.S with type t := t
