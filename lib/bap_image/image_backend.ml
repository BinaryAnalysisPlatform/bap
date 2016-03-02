open Core_kernel.Std
open Bap_types.Std
open Image_common

type perm = R | W | X | Or of perm * perm
[@@deriving bin_io, compare, sexp]

(** A named contiguous part of file with permissions.  *)
module Segment = struct
  type t = {
    name: string;
    perm: perm;         (** segment's permissions  *)
    off: int;
    location : location;
  } [@@deriving bin_io, compare, fields, sexp]
end

(** Symbol definition, that can span several non-contiguous parts of
    memory *)
module Symbol = struct
  type t = {
    name : string;
    is_function : bool;
    is_debug : bool;
    locations : location * location list;
  } [@@deriving bin_io, compare, fields, sexp]
end

module Section = struct
  type t = {
    name : string;
    location : location;
  } [@@deriving bin_io, compare, fields, sexp]
end

module Img = struct
  type t = {
    arch     : arch;
    entry    : addr;
    segments : Segment.t * Segment.t list;
    symbols  : Symbol.t list;
    sections : Section.t list;
  } [@@deriving bin_io, compare, fields, sexp]
end

type t = Bigstring.t -> Img.t option
