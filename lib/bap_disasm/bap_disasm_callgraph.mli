open Core_kernel
open Bap_core_theory
open Bap_types.Std
module Disasm = Bap_disasm_speculative

type t

val empty : t
val equal : t -> t -> bool
val update : t -> Disasm.t -> t KB.t
val entry : t -> addr -> addr
val entries : t -> Set.M(Addr).t

val domain : t KB.domain
