open Core_kernel.Std

type t = Bytes.t [@@deriving bin_io, compare, sexp]

include Container.S0   with type t := t with type elt := char
include Blit.S         with type t := t
include Identifiable.S with type t := t
module To_string   : Blit.S_distinct with type src := t with type dst := string
module From_string : Blit.S_distinct with type src := string with type dst := t

val create : int -> t
val make : int -> char -> t
val init : int -> f:(int -> char) -> t
val empty : t
val length : t -> int
val get : t -> int -> char
val set : t -> int -> char -> unit
val copy : t -> t
val of_string : string -> t
val to_string : t -> string
val extend : t -> int -> int -> t
val fill : t -> int -> int -> char -> unit
val concat : t -> t list -> t
val cat : t -> t -> t
val iteri : t -> f:(int -> char -> unit) -> unit
val map : t -> f:(char -> char) -> t
val mapi : t -> f:(int -> char -> char) -> t
val trim : t -> t
val escaped : t -> t
val index : t -> char -> int
val rindex : t -> char -> int
val index_from : t -> int -> char -> int
val rindex_from : t -> int -> char -> int
val contains : t -> char -> bool
val contains_from : t -> int -> char -> bool
val rcontains_from : t -> int -> char -> bool
val uppercase : t -> t
val lowercase : t -> t
val capitalize : t -> t
val uncapitalize : t -> t

module Unsafe : sig
  [@@@ocaml.warning "-3"]

  val to_string : t -> string
  val of_string : string -> t
  external get  : t -> int -> char = "%string_unsafe_get"
  external set  : t -> int -> char -> unit = "%string_unsafe_set"
  external blit : t -> int -> t -> int -> int -> unit = "caml_blit_string" "noalloc"
  external fill : t -> int -> int -> char -> unit = "caml_fill_string" "noalloc"
end
