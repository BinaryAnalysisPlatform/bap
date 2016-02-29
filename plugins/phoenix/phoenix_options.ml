open Core_kernel.Std
open Bap.Std

type label_format = [ `with_asm | `with_bil | `with_name ]
with sexp

type t = {
  output_folder : string;
  cfg_format : label_format sexp_list;
  no_resolve : bool;
  keep_alive : bool;
  no_inline : bool;
  keep_consts : bool;
  no_optimizations : bool;
} with sexp, fields
