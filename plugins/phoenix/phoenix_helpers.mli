open Core_kernel.Std
open Bap.Std

(** [bil_of_block blk]   *)

module Make(Env : sig
    val options : Phoenix_options.t
    val project : project
    module Target : Target
  end) : sig
  val bil_of_block : block -> bil
  val bil_of_insns : (mem * insn) list -> bil
  val bil_of_insn  : mem * insn -> bil
end
