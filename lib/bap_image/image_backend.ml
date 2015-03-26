open Core_kernel.Std
open Bap_types.Std
open Image_common

type perm = R | W | X | Or of perm * perm
with bin_io, compare, sexp

(** A named contiguous part of file with permissions.  *)
module Section = struct
  type t = {
    name: string;
    perm: perm;         (** section's permissions  *)
    off: int;
    location : location;
  } with bin_io, compare, fields, sexp
end

(** Symbol definition, that can span several non-contiguous parts of
    memory *)
module Sym = struct
  type t = {
    name : string;
    is_function : bool;
    is_debug : bool;
    locations : location * location list;
  } with bin_io, compare, fields, sexp
end

module Tag = struct
  type t = {
    name : string;
    data : string;
    location : location;
  } with bin_io, compare, fields, sexp
end

module Img = struct
  type t = {
    arch     : arch;
    entry    : addr;
    sections : Section.t * Section.t list;
    symbols  : Sym.t list;
    tags     : Tag.t list;
  } with bin_io, compare, fields, sexp
end

type t = Bigstring.t -> Img.t option
