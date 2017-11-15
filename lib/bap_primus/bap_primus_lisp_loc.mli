open Core_kernel

(* a region in a file *)
type range = Parsexp.Positions.range [@@deriving sexp_of]


(* a region in the specified file  *)
type filepos = {
  file  : string;
  range : range;
} [@@deriving compare, sexp_of]

(* a location of a definition *)
type loc =
  | Primitive                   (* defined as a primitive *)
  | Filepos of pos              (* defined in a file *)
[@@deriving sexp_of]
(* definition position that is aware of a preprocessor *)
and pos = {
  def : filepos;                (* a position *)
  src : src;                    (* kind of a definition *)
} [@@deriving sexp_of]

(* how was the definition defined? *)
and src =
  | Ground       (* exactly by the code *)
  | Macro of loc (* as an expansion of a macro defined at loc*)
[@@deriving compare, sexp_of]

type t = loc [@@deriving compare, sexp_of]

val pp : Format.formatter -> t -> unit
val pp_filepos : Format.formatter -> filepos -> unit
val pp_pos : Format.formatter -> pos -> unit

include Comparable.S_plain with type t := t

module Pos : Comparable.S_plain with type t = pos
module Filepos : Comparable.S_plain with type t = filepos

module Current : sig
  val get : Sexp.t -> t
  val range : Sexp.t list -> t
end

(*  *)
module Control : sig
  type positions = Parsexp.Positions.t
  val push : positions -> unit
  val pop : positions -> unit
  val shift : positions -> unit
end
