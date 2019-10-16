open Core_kernel
open Bap_core_theory
open Graphlib.Std
open Bap_types.Std
module Driver = Bap_disasm_driver

type t [@@deriving bin_io]

val empty : t
val equal : t -> t -> bool
val update : t -> Driver.state -> t KB.t
val belongs : t -> entry:addr -> addr -> bool
val entries : t -> Set.M(Addr).t
val siblings : t -> addr -> addr -> bool
val domain : t KB.domain
