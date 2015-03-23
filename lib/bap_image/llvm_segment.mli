open Core_kernel.Std

open Bap_types.Std

type t

val segments_of_binary: Llvm_binary.t -> t list
val name: t -> string
val offset: t -> int64
val address: t -> int64
val length: t -> int
val bitwidth: t -> int
val is_readable: t -> bool
val is_writable: t -> bool
val is_executable: t -> bool
val to_section: t -> Image_backend.Section.t Or_error.t
