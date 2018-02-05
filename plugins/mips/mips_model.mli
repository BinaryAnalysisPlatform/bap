open Core_kernel.Std
open Bap.Std
open Mips_rtl

module type Model = sig
  type t
  val gpr : t String.Map.t
  val gpri : t Int.Map.t
  val fpr : t String.Map.t
  val fpri : t Int.Map.t
  val hi : t
  val lo : t
end

module type Model_exp = sig
  include Model with type t := exp
end

module type MIPS = sig
  module E : Model_exp
  include Model with type t := var

  val mem : var
  val gpr_bitwidth : int
  val fpr_bitwidth : int
end

module MIPS_32 : MIPS
module MIPS_64 : MIPS

module MIPS_32_cpu : CPU
module MIPS_64_cpu : CPU
