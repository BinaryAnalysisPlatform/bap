(** Single symbol in the list of symbols int the object file *)

open Core_kernel.Std

type t

external name : t -> string =
  "symbol_ref_name_stub"

external address : t -> Int64.t =
  "symbol_ref_address_stub"

external is_function : t -> bool =
  "symbol_ref_is_function_stub"

external is_debug : t -> bool =
  "symbol_ref_is_debug_stub"

external size : t -> Int64.t =
  "symbol_ref_size_stub"
