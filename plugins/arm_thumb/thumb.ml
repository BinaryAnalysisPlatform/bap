open Bap_core_theory
open Base
open KB.Syntax

module Defs = Thumb_defs
module Flags = Thumb_flags.Flags

let package = "arm-thumb"

type insns = Defs.insn * (Defs.op list)

module Thumb(Core : Theory.Core) = struct
    open Core
    open Defs
    module Env = Thumb_env.Env

    module Mov = Thumb_mov.Mov(Core)

    let skip = perform Theory.Effect.Sort.bot
    let pass = perform Theory.Effect.Sort.bot

    let nop =
        KB.return @@
        Theory.Effect.empty Theory.Effect.Sort.top

    let reg = Env.load_reg

    let reg_wide = Env.load_reg_wide

    let move eff =
    KB.Object.create Theory.Program.cls >>= fun lbl ->
    blk lbl eff skip

    let ctrl eff pc = 
    Theory.Label.for_addr pc >>= fun lbl ->
    blk lbl pass eff

    let lift_move insn ops =
    let open Mov in
    match insn, ops with
    | `tADC, [dest; src1; src2] -> adc dest src1 src2
    | `tADDi3, [dest; src; imm] -> addi3 dest src imm
    | `tADDi8, [dest; imm] -> addi8 dest imm
    | `tADDrr, [dest; src1; src2] -> addrr dest src1 src2
    | `tADDhirr, [dest; src] -> addhirr dest src
    | `tADR, [dest; imm] -> adr dest imm
    | `tADDrSPi, [dest; imm] -> addrspi dest imm
    | `tADDspi, [imm] -> addspi imm
    | `tMOVr, [dest; src] -> mover dest src
    | `tMOVSr, [dest; src] -> movesr dest src
    | `tMOVi8, [dest; imm] -> movei8 dest imm
    | `tMUL, [dest; src] -> mul dest src
    | `tMVN, [dest; src] -> movenot dest src
    | `tSBC, [dest; src] -> sbc dest src
    | `tSUBi3, [dest; src; imm] -> subi3 dest src imm
    | `tSUBi8, [dest; imm] -> subi8 dest imm
    | `tSUBrr, [dest; src1; src2] -> subrr dest src1 src2
    | `tSUBspi, [imm] -> subspi imm
    | _ -> pass

    let lift ((ins, ops) : insns) : unit Theory.Effect.t KB.t = 
    match ins with
    | #move_insn -> lift_move ins ops |> move
    | _ -> nop

end
