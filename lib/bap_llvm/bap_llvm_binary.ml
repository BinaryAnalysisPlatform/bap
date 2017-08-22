open Core_kernel.Std

(* [@@@ocaml.warning "-3"] *)

(* type t *)

(* external create_binary : Bigstring.t -> t option = "bap_llvm_binary_create_stub" *)

(* external image_arch : t -> string = "bap_llvm_binary_arch_stub" *)

(* external image_entry : t -> int64 = "bap_llvm_binary_entry_stub" *)

(* external segments_number : t -> int = *)
(*   "bap_llvm_binary_segments_number_stub" "noalloc" *)

(* external sections_number : t -> int = *)
(*   "bap_llvm_binary_sections_number_stub" "noalloc" *)

(* external symbols_number  : t -> int = *)
(*   "bap_llvm_binary_symbols_number_stub" "noalloc" *)

(* external segment_name   : t -> int -> string = *)
(*   "bap_llvm_binary_segment_name_stub" *)

(* external segment_addr   : t -> int -> int64  = *)
(*   "bap_llvm_binary_segment_addr_stub" *)

(* external segment_size   : t -> int -> int64  = *)
(*   "bap_llvm_binary_segment_size_stub" *)

(* external segment_offset : t -> int -> int64  = *)
(*   "bap_llvm_binary_segment_offset_stub" *)

(* external segment_is_writable : t -> int -> bool = *)
(*   "bap_llvm_binary_segment_is_writable_stub" "noalloc" *)

(* external segment_is_readable : t -> int -> bool = *)
(*   "bap_llvm_binary_segment_is_readable_stub" "noalloc" *)

(* external segment_is_executable : t -> int -> bool = *)
(*   "bap_llvm_binary_segment_is_executable_stub" "noalloc" *)

(* external symbol_name : t -> int -> string = *)
(*   "bap_llvm_binary_symbol_name_stub" *)

(* external symbol_addr : t -> int -> int64  = *)
(*   "bap_llvm_binary_symbol_addr_stub" *)

(* external symbol_size : t -> int -> int64  = *)
(*   "bap_llvm_binary_symbol_size_stub" *)

(* external symbol_is_fun : t -> int -> bool = *)
(*   "bap_llvm_binary_symbol_is_fun_stub" "noalloc" *)

(* external symbol_is_debug : t -> int -> bool = *)
(*   "bap_llvm_binary_symbol_is_debug_stub" "noalloc" *)

(* external section_name : t -> int -> string = *)
(*   "bap_llvm_binary_section_name_stub" *)

(* external section_addr : t -> int -> int64  = *)
(*   "bap_llvm_binary_section_addr_stub" *)

(* external section_size : t -> int -> int64  = *)
(*   "bap_llvm_binary_section_size_stub" *)

external bap_llvm_load : Bigstring.t -> string =
  "bap_llvm_load_stub"
