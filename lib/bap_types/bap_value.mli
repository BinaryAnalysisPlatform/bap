open Core_kernel.Std
open Bap_common

type t with sexp_of
type 'a tag with sexp_of

val create : 'a tag -> 'a -> t
val get : 'a tag -> t -> 'a option
val is  : 'a tag -> t -> bool
val tagname : t -> string

module Map : sig
  type t
  val empty : t
  val is_empty : t -> bool
  val set : t -> 'a tag -> 'a -> t
  val mem : t -> 'a tag -> bool
  val find : t -> 'a tag -> 'a option
  val add : t -> 'a tag -> 'a -> [`Ok of t | `Duplicate]
  val change : t -> 'a tag -> ('a option -> 'a option) -> t
end

module Tag : sig
  type 'a t = 'a tag with sexp_of
  val to_string : 'a tag -> string
  val register : string -> ('a -> Sexp.t) -> 'a tag
end

include Printable with type t := t
