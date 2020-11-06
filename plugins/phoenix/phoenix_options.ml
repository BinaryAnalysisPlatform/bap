open Core_kernel
open Bap.Std

type label_format = [ `with_asm | `with_bil | `with_name ]
[@@deriving sexp]

type t = {
  output_folder : string;
  cfg_format : label_format list;
  no_resolve : bool;
  keep_alive : bool;
  no_inline : bool;
  keep_consts : bool;
  no_optimizations : bool;
} [@@deriving sexp, fields]
