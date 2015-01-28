open Core_kernel.Std
open Bap_types.Std

(** [insn mem basic] takes a basic instruction and a memory and
    returns a sequence of BIL statements. *)
val insn : Bap_memory.t -> ('a,'k) Bap_disasm_basic.insn -> stmt list Or_error.t
