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
    vsize : int;        (** virtual size  *)
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

module Img = struct
  type t = {
    arch : arch;
    addr_size: Word_size.t;
    endian   : endian;
    entry    : addr;
    sections : Section.t * Section.t list;
    symbols  : Sym.t list;
  } with fields
end

type t = Bigstring.t -> Img.t option
