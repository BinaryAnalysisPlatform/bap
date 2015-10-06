open Core_kernel.Std

type t = {
  src : string option;
  addr : string;
  max_insn : int option;
  arch : string;
  show_insn_size : bool;
  insn_formats : [ `adt | `asm | `sexp ] list;
  bil_formats : [ `adt | `bil | `binprot | `json | `pb | `sexp | `xml ] list;
  bir_formats : [ `binprot | `bir | `sexp ] list;
  show_kinds: bool;
} with sexp, fields

module type Provider = sig
  val options : t
end
