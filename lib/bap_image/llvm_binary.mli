(** LLVM binary *)

open Core_kernel.Std
open Bap_types.Std

type t

external create : Bigstring.t -> t =
  "llvm_binary_create_stub"

external arch : t -> arch =
  "llvm_binary_arch_stub"

external symbols : t -> Symbol_ref.t list =
  "llvm_binary_symbols_stub"

external sections : t -> Section_ref.t list =
  "llvm_binary_sections_stub"
