open Core_kernel
open Bap_core_theory
open Regular.Std
open Bap_common

val of_string : string -> arch option

val addr_size : arch -> addr_size

val endian : arch -> endian

val slot : (Theory.program, arch option) KB.slot

include Regular.S with type t := arch
