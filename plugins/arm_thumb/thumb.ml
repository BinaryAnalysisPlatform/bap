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

    let bool_as_bitv b = ite b (int Env.value Bitvec.one) (int Env.value Bitvec.zero)

    let word_as_bitv w = (int Env.value (Bap.Std.Word.to_bitvec w))

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

    let move_reg ?lreg:(lreg = reg) dest src = set (lreg dest) (var (lreg src))

    let set_reg ?lreg:(lreg = reg) dest imm = 
        set (lreg dest) (word_as_bitv imm)

    let mover dest src = match dest, src with
    | `Reg d, `Reg s -> (move_reg ~lreg:reg_wide d s) |> move
    | _ -> raise (Lift_Error "dest or src is not a register")

    let movesr dest src = match dest, src with
    | `Reg d, `Reg s -> seq (move_reg d s) (Flags.set_nzf (reg d)) |> move
    | _ -> raise (Lift_Error "dest or src is not a register")

    let movei8 dest imm = match dest, imm with
    | `Reg d, `Imm v -> seq (set_reg d v) (Flags.set_nzf (reg d)) |> move
    | _ -> raise (Lift_Error "incorrect operands")

    let mul dest src = match dest, src with
    | `Reg d, `Reg s -> 
        let (dest, src) = (reg d, reg s) in
        seq 
            (set dest (mul (var dest) (var src))) 
            (Flags.set_nzf dest)
            |> move
    | _ -> raise (Lift_Error "dest or src is not a register")

    let addi3 dest src imm = match dest, src, imm with
    | `Reg d, `Reg s, `Imm v -> 
        let (d, s1, s2) = (reg d, reg s, word_as_bitv v) in
        seq 
            (set d (add (var s1) s2)) 
            (Flags.set_add (var s1) s2 d) 
            |> move
    | _ -> raise (Lift_Error "incorrect operands")

    let addi8 dest imm = match dest, imm with
    | `Reg d, `Imm v -> 
        let (d, s) = (reg d, word_as_bitv v) in
        seq 
            (set d (add (var d) s)) 
            (Flags.set_add (var d) s d) 
            |> move
    | _ -> raise (Lift_Error "incorrect operands")

    let addrr d s1 s2 = match d, s1, s2 with
    | `Reg d, `Reg s1, `Reg s2 -> 
        let (d, s1, s2) = (reg d, reg s1, reg s2) in
        seq 
            (set d (add (var s1) (var s2))) 
            (Flags.set_add (var s1) (var s2) d) 
            |> move
    | _ -> raise (Lift_Error "operands must be registers")

    let adc d s1 s2 = match d, s1, s2 with
    | `Reg d, `Reg s1, `Reg s2 -> 
        let (d, s1, s2) = (reg d, reg s1, reg s2) in
        seq 
            (set d (add (add (var s1) (var s2)) (bool_as_bitv (var Env.cf)))) 
            (Flags.set_adc (var s1) (var s2) d) 
            |> move
    | _ -> raise (Lift_Error "operands must be registers")

    let lift_move insn ops =
    match insn, ops with
    | `tADC, [dest; src1; src2] -> adc dest src1 src2
    | `tADDrr, [dest; src1; src2] -> addrr dest src1 src2
    | `tMOVr, [dest; src] -> mover dest src
    | `tMOVSr, [dest; src] -> movesr dest src
    | `tMOVi8, [dest; imm] -> movei8 dest imm
    | `tMUL, [dest; src] -> mul dest src
    | _ -> nop

    let lift ((ins, ops) : insns) : unit Theory.Effect.t KB.t = 
    match ins with
    | #move_insn -> lift_move ins ops
    | _ -> nop

end
