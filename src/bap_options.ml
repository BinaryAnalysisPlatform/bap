open Core_kernel
open Bap.Std

type source = Bap_source_type.t [@@deriving sexp]
type fmt_spec = Bap_fmt_spec.t [@@deriving sexp]

type t = {
  filename        : string;
  disassembler    : string;
  loader          : string;
  dump            : fmt_spec list;
  source          : source;
  verbose         : bool;
  passes          : string list;
} [@@deriving sexp, fields]
