open Core_kernel.Std

type demangle = [`program of string | `internal] with sexp
type insn_format = [ `with_asm | `with_bil  ] with sexp
type dump_format = [insn_format | `with_bir] with sexp
type label_format = [`with_name | insn_format] with sexp

type t = {
  filename : string;
  loader : string;
  symsfile : string sexp_option;
  cfg_format : label_format sexp_list;
  output_phoenix : string option;
  output_dump : dump_format sexp_list;
  dump_symbols : string option option;
  demangle : demangle sexp_option;
  no_resolve : bool;
  keep_alive : bool;
  no_inline : bool;
  keep_consts : bool;
  no_optimizations : bool;
  binaryarch : string option;
  verbose : bool;
  bw_disable : bool;
  bw_length : int;
  bw_threshold : float;
  print_symbols : [`with_name | `with_addr | `with_size] list;
  use_ida : string option option;
  sigfile : string option;
  plugins : string list;
  emit_ida_script : string option;
  load_path : string list;
  emit_attr : string list;
} with sexp, fields

module type Provider = sig
  val options : t
end
