open Core_kernel
open Elf_types

(** [section_name data elf section] extracts section name from data  *)
val section_name : Bigstring.t -> elf -> section -> string Or_error.t


val string_of_section : Bigstring.t -> section -> string Or_error.t
