open Core_kernel.Std
open Bap.Std

(** [bil_of_block blk]   *)

module Make(Env : Printing.Env) : sig
  val bil_of_block : block -> bil
  val bil_of_insns : (mem * insn) list -> bil
  val bil_of_insn  : mem * insn -> bil
end
