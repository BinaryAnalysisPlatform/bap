(** LLVM binary *)

open Core_kernel.Std
open Bap_types.Std

type t

external create : Bigstring.t -> t =
  "llvm_binary_create_stub"

external arch : t -> arch =
  "llvm_binary_arch_stub"

external entry : t -> int64 =
  "llvm_binary_entry_stub"

external segments : t -> Llvm_binary_segment.t list =
  "llvm_binary_segments_stub"

external symbols : t -> Llvm_binary_symbol.t list =
  "llvm_binary_symbols_stub"

external sections : t -> Llvm_binary_section.t list =
  "llvm_binary_sections_stub"
