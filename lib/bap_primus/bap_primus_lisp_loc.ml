open Core_kernel.Std
open Format

type range = Parsexp.Positions.range [@@deriving compare, sexp_of]
type loc = {
  file  : string;
  range : range;
} [@@deriving compare, sexp_of]


let pp ppf {file; range={start_pos=s; end_pos=e}}  =
  let len = e.offset - s.offset in
  fprintf ppf "file %S, line %d, characters %d-%d"
    file s.line s.col (s.col+len)



include Comparable.Make_plain(struct
    type t = loc [@@deriving compare, sexp_of]
  end)

type t = loc [@@deriving compare, sexp_of]
