open Core_kernel.Std

module Std : sig
  type x86_syntax = [`att | `intel] [@@deriving sexp]

  val llvm_version : string
  val init_disassembler : ?x86_syntax:x86_syntax -> unit -> unit Or_error.t
  val init_loader : ?base:int64 -> unit -> unit
end
