(** LLVM object file *)

open Core_kernel.Std
open Bap_types.Std

type t

external create : Bigstring.t -> t =
  "object_file_create_stub"

external arch : t -> arch =
  "object_file_arch_stub"

external symbols : t -> Symbol_ref.t list =
  "object_file_symbols_stub"

external sections : t -> Section_ref.t list =
  "object_file_sections_stub"
