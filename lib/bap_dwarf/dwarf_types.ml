(** Basic type declarations for DWARF format.  *)
open Core_kernel
open Bap.Std

type leb128 = Dwarf_leb128.t [@@deriving bin_io, compare, sexp]

(** File sections  *)
module Section = struct
  type t =
    | Info
    | Abbrev
    | Str
  [@@deriving sexp, bin_io, compare, variants]
end

(** Debug Entry Tag  *)
module Tag = struct
  type t =
    | Compile_unit
    | Partial_unit
    | Subprogram
    | Entry_point
    | Inlined_subroutine
    | Unknown of int
  [@@deriving sexp, bin_io, compare, variants]
end


(** Attribute  *)
module Attr = struct
  type t =
    | Name
    | Low_pc
    | High_pc
    | Entry_pc
    | Unknown of int
  [@@deriving sexp, bin_io, compare, variants]
end

type lenspec =
  | Leb128
  | One
  | Two
  | Four
  | Eight
[@@deriving sexp, bin_io, compare]

(** Attribute form  *)
module Form = struct
  type t =
    | Addr
    | String
    | Block of lenspec
    | Const of lenspec
    | Flag_present
    | Strp
    | Ref of lenspec
    | Indirect
    | Offset
    | Expr
    | Sig
  [@@deriving sexp, bin_io, compare, variants]
end

type tag  = Tag.t  [@@deriving sexp, bin_io, compare]
type attr = Attr.t [@@deriving sexp, bin_io, compare]
type form = Form.t [@@deriving sexp, bin_io, compare]
type section = Section.t [@@deriving sexp, bin_io, compare]
