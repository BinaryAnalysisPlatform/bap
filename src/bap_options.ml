open Core_kernel.Std
open Bap.Std

type source = Bap_source_type.t with sexp
type fmt_spec = Bap_fmt_spec.t with sexp

type t = {
  filename : string;
  disassembler : string;
  loader : string;
  dump : fmt_spec list;
  source : source;
  verbose : bool;
  brancher : string option;
  symbolizers : string list;
  rooters : string list;
  symbols : string list;
  reconstructor : string option;
  passes : string list;
} with sexp, fields
