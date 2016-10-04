open Core_kernel.Std
open Bap.Std
open Regular.Std

include X86_opcode_btx
include X86_opcode_cmps
include X86_opcode_cmpxchg
include X86_opcode_ins
include X86_opcode_lods
include X86_opcode_mov
include X86_opcode_movs
include X86_opcode_outs
include X86_opcode_scas
include X86_opcode_stos

type t = [
  | btx
  | cmps
  | cmpxchg
  | ins
  | lods
  | mov
  | movs
  | outs
  | scas
  | stos
] [@@deriving bin_io, sexp, compare, enumerate]
