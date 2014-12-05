open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_arm_types

(** [insn mem basic] takes a basic instruction and a memory and
    returns a sequence of BIL statements. *)
val insn : mem -> ('a,'k) Basic.insn -> stmt list Or_error.t
