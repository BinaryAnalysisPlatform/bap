open Core_kernel.Std
open Elf_types

val from_bigstring : ?pos:int -> ?len:int ->  Bigstring.t -> elf Or_error.t
