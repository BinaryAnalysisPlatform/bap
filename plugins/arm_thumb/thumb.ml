open Bap_core_theory
open Base
open KB.Syntax

module Defs = Thumb_defs
module Flags = Thumb_flags.Flags

let package = "arm-thumb"

type insns = Defs.insn * (Defs.op list)

exception Lift_Error of string

module Thumb(Core : Theory.Core) = struct
    open Core
    open Defs
    module Env = Thumb_env.Env

    module Flags = Flags(Core)

    let skip = perform Theory.Effect.Sort.bot
    let pass = perform Theory.Effect.Sort.bot

    let nop =
        KB.return @@
        Theory.Effect.empty Theory.Effect.Sort.top

    let reg = Env.load_reg

    let move eff =
    KB.Object.create Theory.Program.cls >>= fun lbl ->
    blk lbl eff skip

    let ctrl eff pc = 
    Theory.Label.for_addr pc >>= fun lbl ->
    blk lbl pass eff

    let move_reg dest src = set (reg dest) (var (reg src))

    let set_reg dest imm = 
        set (reg dest) (int Env.value (Bap.Std.Word.to_bitvec imm))

    let mover dest src = match dest, src with
    | `Reg d, `Reg s -> (move_reg d s) |> move
    | _ -> raise (Lift_Error "dest or src is not a register")

    let movei8 dest imm = match dest, imm with
    | `Reg d, `Imm v -> seq (set_reg d v) (Flags.set_nzf (reg d)) |> move
    | _ -> raise (Lift_Error "incorrect oprands")

    let addrr d s1 s2 = match d, s1, s2 with
    | `Reg d, `Reg s1, `Reg s2 -> 
        let (d, s1, s2) = (reg d, reg s1, reg s2) in
        seq 
            (set d (add (var s1) (var s2))) 
            (Flags.set_add (var s1) (var s2) d) 
            |> move
    | _ -> raise (Lift_Error "oprends must be registers")

    let lift_move insn ops =
    match insn, ops with
    | `tMOVr, [dest; src] -> mover dest src
    | `tMOVi8, [dest; imm] -> movei8 dest imm
    | `tADDrr, [dest; src1; src2] -> addrr dest src1 src2
    | _ -> nop

    let lift ((ins, ops) : insns) : unit Theory.Effect.t KB.t = 
    match ins with
    | #move_insn -> lift_move ins ops
    | _ -> nop

end
