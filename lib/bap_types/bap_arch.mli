open Core_kernel
open Bap_knowledge
open Regular.Std
open Bap_common

module Semantics = Bap_types_semantics

val of_string : string -> arch option

val addr_size : arch -> addr_size

val endian : arch -> endian

val slot : (Semantics.cls, arch option) Knowledge.slot

include Regular.S with type t := arch
