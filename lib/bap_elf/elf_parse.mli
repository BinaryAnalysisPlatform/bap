open Core_kernel[@@warning "-D"]
open Elf_types

val from_bigstring : ?pos:int -> ?len:int ->  Bigstring.t -> elf Or_error.t
