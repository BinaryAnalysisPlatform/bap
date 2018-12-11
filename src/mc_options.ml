open Core_kernel.Std

type t = {
  disassembler : string;
  src : string option;
  addr : string;
  only_one : bool;
  arch : string;
  show_insn_size : bool;
  insn_formats : string list;
  semantics : string list option;
  bil_formats : string list;
  bir_formats : string list;
  show_kinds: bool;
} [@@deriving sexp, fields]

module type Provider = sig
  val options : t
end
