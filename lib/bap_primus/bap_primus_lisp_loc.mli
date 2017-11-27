open Core_kernel

(* a region in a file *)
type range = Parsexp.Positions.range [@@deriving sexp_of]


(* a region in the specified file  *)
type loc = {
  file  : string;
  range : range;
} [@@deriving compare, sexp_of]

type t = loc [@@deriving compare, sexp_of]

val pp : Format.formatter -> t -> unit

include Comparable.S_plain with type t := t
