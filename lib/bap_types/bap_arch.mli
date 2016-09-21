open Core_kernel.Std
open Regular.Std
open Bap_common

val of_string : string -> arch option

val addr_size : arch -> addr_size

val endian : arch -> endian

include Regular.S with type t := arch
