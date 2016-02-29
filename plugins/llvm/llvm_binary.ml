(** LLVM binary *)

open Core_kernel.Std

module LLVM = Llvm_types

type t

external create : Bigstring.t -> t =
  "llvm_binary_create_stub"

external arch : t -> string =
  "llvm_binary_arch_stub"

external entry : t -> int64 =
  "llvm_binary_entry_stub"

external segments : t -> LLVM.Segment.t list =
  "llvm_binary_segments_stub"

external symbols : t -> LLVM.Symbol.t list =
  "llvm_binary_symbols_stub"

external sections : t -> LLVM.Section.t list =
  "llvm_binary_sections_stub"
