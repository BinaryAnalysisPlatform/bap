open Core_kernel.Std

type t = {
  src : string option;
  addr : string;
  max_insn : int option;
  arch : string;
  show_insn_size : bool;
  insn_formats : string list;
  bil_formats : string list;
  bir_formats : string list;
  show_kinds: bool;
} with sexp, fields

module type Provider = sig
  val options : t
end
