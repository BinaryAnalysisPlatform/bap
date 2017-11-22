open Core_kernel.Std
open Format

type range = Parsexp.Positions.range [@@deriving compare, sexp_of]
type filepos = {
  file  : string;
  range : range;
} [@@deriving compare, sexp_of]

type loc =
  | Primitive
  | Filepos of pos
[@@deriving compare, sexp_of]

and pos = {
  def : filepos;
  src : src;
}
[@@deriving compare, sexp_of]

and src =  Ground | Macro of loc
[@@deriving compare, sexp_of]

let pp_filepos ppf {file; range={start_pos=s; end_pos=e}}  =
  let len = e.offset - s.offset in
  fprintf ppf "file %S, line %d, characters %d-%d"
    file s.line s.col (s.col+len)

let rec pp_pos ppf {def; src} =
  fprintf ppf "%a" pp_filepos def;
  match src with
  | Ground -> ()
  | Macro loc ->
    fprintf ppf "@\nexpanded from %a" pp_loc loc
and pp_loc ppf loc = match loc with
  | Primitive -> fprintf ppf "<primitive>"
  | Filepos epos -> fprintf ppf "%a" pp_pos epos

let pp = pp_loc

let create file range = FilePpos {
  src = Ground;
  def = {file; range}}


include Comparable.Make_plain(struct
    type t = loc [@@deriving compare, sexp_of]
end)

type t = loc


module Pos = Comparable.Make_plain(struct
    type t = pos [@@deriving compare, sexp_of]
end)

module Filepos = Comparable.Make_plain(struct
    type t = filepos [@@deriving compare, sexp_of ]
  end)
