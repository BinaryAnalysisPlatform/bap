open Core_kernel.Std


(** Initialize LLVM backend.

    So far we keep the interface very tight, though later we may
    extend it by publishing more functions.

*)
module Std : sig
  type x86_syntax = [`att | `intel] [@@deriving sexp]


  (** [llvm_version] *)
  val llvm_version : string

  (** [init_disassembler ()] initializes and registers the LLVM based disassembler.  *)
  val init_disassembler : ?x86_syntax:x86_syntax -> unit -> unit Or_error.t


  (** [init-loader ()] initializes and registers the LLVM based
      loader.

      @param base a base address to use. If not specified, then a base
      address specified by the binary is used. If there is no such
      address, then some arbitrary constant will be used. *)
  val init_loader : ?base:int64 -> unit -> unit
end
