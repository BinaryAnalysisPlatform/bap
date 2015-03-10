(** Single section in the list of sections int the object file *)

open Core_kernel.Std

type t

external name : t -> string =
  "section_ref_name_stub"

external address : t -> Int64.t =
  "section_ref_address_stub"

external size : t -> Int64.t =
  "section_ref_size_stub"

external is_readable : t -> bool =
  "section_ref_is_readable_stub"

external is_writable : t -> bool =
  "section_ref_is_writable_stub"

external is_executable : t -> bool =
  "section_ref_is_executable_stub"

external offset : t -> int =
  "section_ref_offset_stub"
