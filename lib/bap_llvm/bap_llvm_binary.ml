open Core_kernel.Std
open Bap_llvm_types

type t

external create : Bigstring.t -> t = "llvm_binary_create_stub"
external arch : t -> string = "llvm_binary_arch_stub"
external entry : t -> int64 = "llvm_binary_entry_stub"
external segments : t -> segment list = "llvm_binary_segments_stub"
external symbols : t -> symbol list = "llvm_binary_symbols_stub"
external sections : t -> section list = "llvm_binary_sections_stub"
