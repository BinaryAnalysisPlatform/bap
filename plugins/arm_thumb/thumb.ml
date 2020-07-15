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
    module Mem = Thumb_mem.Mem(Core)
    module Utils = Thumb_util.Utils(Core)

    open Utils

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

    let lift_mem insn ops =
    let open Defs in
    let open Mem in
    match insn, ops with
    | `tLDRi, [dest; src; imm] -> 
        lift_mem_single dest src ~src2:imm Ld W
    | `tLDRr, [dest; src1; src2] -> 
        lift_mem_single dest src1 ~src2 Ld W
    | `tLDRpci, [dest; imm] -> 
        lift_mem_single dest `PC ~src2:imm Ld W
    | `tLDRspi, [dest; imm] -> 
        lift_mem_single dest `SP ~src2:imm Ld W
    | `tLDRBi, [dest; src; imm] -> 
        lift_mem_single dest src ~src2:imm Ld B
    | `tLDRBr, [dest; src1; src2] -> 
        lift_mem_single dest src1 ~src2 Ld B
    | `tLDRHi, [dest; src; imm] -> 
        lift_mem_single dest src ~src2:imm Ld H
    | `tLDRHr, [dest; src1; src2] -> 
        lift_mem_single dest src1 ~src2 Ld H
    | `tLDRSB, [dest; src1; src2] -> 
        lift_mem_single dest src1 ~src2 Ld B ~sign:true
    | `tLDRSH, [dest; src1; src2] -> 
        lift_mem_single dest src1 ~src2 Ld H ~sign:true
    | `tSTRi, [dest; src; imm] -> 
        lift_mem_single dest src ~src2:imm St W
    | `tSTRr, [dest; src1; src2] -> 
        lift_mem_single dest src1 ~src2 St W
    | `tSTRspi, [dest; imm] -> 
        lift_mem_single dest `SP ~src2:imm St W
    | `tSTRBi, [dest; src; imm] -> 
        lift_mem_single dest src ~src2:imm St B
    | `tSTRBr, [dest; src1; src2] -> 
        lift_mem_single dest src1 ~src2 St B
    | `tSTRHi, [dest; src; imm] -> 
        lift_mem_single dest src ~src2:imm St H
    | `tSTRHr, [dest; src1; src2] -> 
        lift_mem_single dest src1 ~src2 St H
    | _ -> pass

    let lift ((ins, ops) : insns) : unit Theory.Effect.t KB.t = 
    match ins with
    | #move_insn -> lift_move ins ops |> move
    | #mem_insn -> lift_mem ins ops |> move
    | _ -> nop

end
