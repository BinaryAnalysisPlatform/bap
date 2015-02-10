open Core_kernel.Std

type demangle = [`program of string | `internal] with sexp
type insn_format = [ `with_asm | `with_bil ] with sexp
type label_format = [`with_name | insn_format] with sexp

type t = {
  filename : string;
  symsfile : string sexp_option;
  cfg_format : label_format sexp_list;
  output_phoenix : string option;
  output_dump : insn_format sexp_list;
  demangle : demangle sexp_option;
  no_resolve : bool;
  keep_alive : bool;
  no_inline : bool;
  keep_consts : bool;
  no_optimizations : bool;
} with sexp, fields

module type Provider = sig
  val options : t
end
