open Core_kernel.Std
open Bap.Std

module Hardware : sig

  (** general purpose registers bitwidth *)
  val gpr_bitwidth : int

  (** floating point registers bitwidth *)
  val fpr_bitwidth : int

  (** vector registers bitwidth *)
  val vr_bitwidth  : int

  (** condition register bitwidth *)
  val cr_bitwidth  : int

  (** xer register bitwidth *)
  val xer_bitwidth : int

  (** link register bitwidth *)
  val lr_bitwidth  : int

  (** count register bitwidth *)
  val ctr_bitwidth : int

  (** target address register bitwidth *)
  val tar_bitwidth : int

  (** all general purpose registers *)
  val gpr : Var.Set.t

  (** all floating point registers *)
  val fpr : Var.Set.t

  (** all vector registers *)
  val vr : Var.Set.t

  (** xer register bits *)
  val xer : var

  (** conditional register bits *)
  val cr : var Int.Map.t

  (** count register  *)
  val ctr : var

  (** link register  *)
  val lr : var

  (** target register  *)
  val tar : var

  (** condition register bits *)
  val cr : var Int.Map.t

  (** condition register fields *)
  val cr_fields : (var * var * var * var) String.Map.t

  (** fixed precision flags *)
  val so : var   (** summary overflow        *)
  val ca : var   (** carry flag              *)
  val ov : var   (** overflow flag           *)
  val ca32 : var (** carry out of 32 bits    *)
  val ov32 : var (** overflow of 32 bits     *)

end

module PowerPC_32 : sig
  include module type of Hardware
  val mem : var
end

module Power_PC_64 : sig
  include module type of Hardware
  val mem : var
end

module PowerPC_32_cpu : CPU
module PowerPC_64_cpu : CPU
