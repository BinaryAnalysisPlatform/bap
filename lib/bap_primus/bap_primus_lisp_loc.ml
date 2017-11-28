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

let merge_pos merge p1 p2 = Parsexp.Positions.{
    line = merge p1.line p2.line;
    col = merge p1.col p2.col;
    offset = merge p1.offset p2.offset;
  }

let merge p1 p2 =
  if p1.file <> p2.file
  then invalid_arg "Loc: can't merge locations from different files";
  Parsexp.Positions.{
    p1 with range = {
      start_pos = merge_pos min p1.range.start_pos p2.range.start_pos;
      end_pos   = merge_pos max p1.range.end_pos p2.range.end_pos;
    }
  }


include Comparable.Make_plain(struct
    type t = loc [@@deriving compare, sexp_of]
  end)

type t = loc [@@deriving compare, sexp_of]
