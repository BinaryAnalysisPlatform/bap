open Core_kernel.Std
open Regular.Std
open Bap_common

module Export : sig
  val bool_t  : typ
  val reg8_t  : typ
  val reg16_t : typ
  val reg32_t : typ
  val reg64_t : typ
  val reg128_t: typ
  val reg256_t: typ
  val mem32_t : size -> typ
  val mem64_t : size -> typ
end

include Regular.S with type t := typ
