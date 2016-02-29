(** Extends [typ] interface.  *)
open Core_kernel.Std
open Regular.Std
open Bap_common

(** This definitions will be placed in a global namespace after you
    open an [Bap_types.Std] module *)
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

include Regular with type t := typ
