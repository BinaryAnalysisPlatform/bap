(** bring to scope some basic types like [mem] and [table] *)
include Image_internal_std

module Image   = Bap_image
module Section = Image.Sec
module Symbol  = Image.Sym

type image = Image.t
type symbol  = Symbol.t with bin_io, compare, sexp
type section = Section.t with bin_io, compare, sexp
