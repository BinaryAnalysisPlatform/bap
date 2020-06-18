open Bap_core_theory
open Base
open KB.Syntax
open A64_defs
open A64_env_registers

module Env = struct
    include A64_env_registers.Env_registers
    
    type byte_t
    type reg_single_t
    type half_byte_t

    let bit_sort : Theory.Bool.t Theory.Value.sort = Theory.Bool.t

    (** grps are defined by 5-bit indices *)(* TODO *)
    let reg_single : reg_single_t Theory.Bitv.t Theory.Value.sort = Theory.Bitv.define 5

    let half_byte : half_byte_t Theory.Bitv.t Theory.Value.sort = Theory.Bitv.define 4

    let byte_sort : byte_t Theory.Bitv.t Theory.Value.sort = Theory.Bitv.define 8

    let mem_sort = Theory.Mem.define bv64_sort byte_sort

    let memory = Theory.Var.define mem_sort "memory"

    let nf = Theory.Var.define bit_sort "nf"
    let zf = Theory.Var.define bit_sort "zf"
    let cf = Theory.Var.define bit_sort "cf"
    let vf = Theory.Var.define bit_sort "vf"

end