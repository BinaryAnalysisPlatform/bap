open Core_kernel.Std
open Bap.Std

(** [bil_of_block blk]   *)

module Make(Env : Printing.Env) : sig
  val bil_of_block : block -> bil
end
