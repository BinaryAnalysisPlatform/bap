open Bap.Std
open Regular.Std


(** insn opcode.

    In contradicition with BAP insn, the RISC-V one is just an opcode,
    without operands. *)
type t = Riscv_types.insn [@@deriving bin_io, compare, sexp]


(** [create insn] translate from BAP [insn]  *)
val create : insn -> t option


(** [of_basic insn] translate from BAP basic [insn]  *)
val of_basic : ('a,'b) Disasm_expert.Basic.insn -> t option

include Regular.S with type t := t
