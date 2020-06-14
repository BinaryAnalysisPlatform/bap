open Bap_core_theory
open Base
open KB.Syntax

module Env  = Thumb_env.Env
module Flags = Thumb_flags.Flags

exception Lift_Error = Thumb_defs.Lift_Error

module Mov(Core : Theory.Core) = struct
open Core

    module Flags = Flags(Core)

    let reg = Env.load_reg

    let reg_wide = Env.load_reg_wide

    let bool_as_bitv b = ite b (int Env.value Bitvec.one) (int Env.value Bitvec.zero)

    let word_as_bitv w = (int Env.value (Bap.Std.Word.to_bitvec w))

    let bitv_of imm = word_as_bitv (Bap.Std.Word.of_int 32 imm)

    let move_reg ?lreg:(lreg = reg) dest src = set (lreg dest) (var (lreg src))

    let set_reg ?lreg:(lreg = reg) dest imm = 
        set (lreg dest) (word_as_bitv imm)

    let mover dest src = match dest, src with
    | `Reg d, `Reg s -> (move_reg ~lreg:reg_wide d s) 
    | _ -> raise (Lift_Error "dest or src is not a register")

    let movesr dest src = match dest, src with
    | `Reg d, `Reg s -> seq (move_reg d s) (Flags.set_nzf (reg d)) 
    | _ -> raise (Lift_Error "dest or src is not a register")

    let movei8 dest imm = match dest, imm with
    | `Reg d, `Imm v -> seq (set_reg d v) (Flags.set_nzf (reg d)) 
    | _ -> raise (Lift_Error "incorrect operands")

    let movenot dest src = match dest, src with
    | `Reg d, `Reg s -> 
        let (dest, src) = (reg d, reg s) in
        seq 
            (set dest (not (var src))) 
            (Flags.set_nzf dest)
    | _ -> raise (Lift_Error "dest or src is not a register")

    let mul dest src = match dest, src with
    | `Reg d, `Reg s -> 
        let (dest, src) = (reg d, reg s) in
        seq 
            (set dest (mul (var dest) (var src))) 
            (Flags.set_nzf dest)
    | _ -> raise (Lift_Error "dest or src is not a register")

    let addi3 dest src imm = match dest, src, imm with
    | `Reg d, `Reg s, `Imm v -> 
        let (d, s1, s2) = (reg d, reg s, word_as_bitv v) in
        seq 
            (set d (add (var s1) s2)) 
            (Flags.set_add (var s1) s2 d) 
    | _ -> raise (Lift_Error "incorrect operands")

    let subi3 dest src imm = match dest, src, imm with
    | `Reg d, `Reg s, `Imm v -> 
        let (d, s1, s2) = (reg d, reg s, word_as_bitv v) in
        seq 
            (set d (sub (var s1) s2)) 
            (Flags.set_sub (var s1) s2 d) 
    | _ -> raise (Lift_Error "incorrect operands")

    (* a temp value is introduced here *)
    let addi8 dest imm = match dest, imm with
    | `Reg d, `Imm v -> 
        let (d, s) = (reg d, word_as_bitv v) in
        seq 
            (seq
                (set Env.tmp (var d))
                (set d (add (var d) s)) 
            )
            (Flags.set_add (var Env.tmp) s d) 
    | _ -> raise (Lift_Error "incorrect operands")

    let subi8 dest imm = match dest, imm with
    | `Reg d, `Imm v -> 
        let (d, s) = (reg d, word_as_bitv v) in
        seq 
            (seq
                (set Env.tmp (var d))
                (set d (sub (var d) s)) 
            )
            (Flags.set_sub (var Env.tmp) s d) 
    | _ -> raise (Lift_Error "incorrect operands")

    let addrr d s1 s2 = match d, s1, s2 with
    | `Reg d, `Reg s1, `Reg s2 -> 
        let (d, s1, s2) = (reg d, reg s1, reg s2) in
        seq 
            (set d (add (var s1) (var s2)))
            (Flags.set_add (var s1) (var s2) d) 
    | _ -> raise (Lift_Error "operands must be registers")

    let subrr d s1 s2 = match d, s1, s2 with
    | `Reg d, `Reg s1, `Reg s2 -> 
        let (d, s1, s2) = (reg d, reg s1, reg s2) in
        seq 
            (set d (sub (var s1) (var s2))) 
            (Flags.set_sub (var s1) (var s2) d) 
    | _ -> raise (Lift_Error "operands must be registers")

    let addhirr d s = match d, s with
    | `Reg d, `Reg s -> 
        let (d, s) = (reg_wide d, reg_wide s) in
            set d (add (var d) (var s)) 
    | _ -> raise (Lift_Error "operands must be registers")

    (* Rd = (PC and 0xfffffffc) + (imm << 2) *)
    let adr dest imm = match dest, imm with
    | `Reg d, `Imm v -> 
        let shift x = bitv_of 2 |> shiftl b0 x in
        let (d, s) = (reg d, word_as_bitv v) in
        set d (add (logand (var Env.pc) (bitv_of 0xFFFFFFFC)) (shift s))
    | _ -> raise (Lift_Error "incorrect operands")

    let addrspi dest imm = match dest, imm with
    | `Reg d, `Imm v -> 
        let shift x = bitv_of 2 |> shiftl b0 x in
        let (d, s) = (reg d, word_as_bitv v) in
        set d (add (var Env.sp) (shift s))
    | _ -> raise (Lift_Error "incorrect operands")

    let addspi imm = match imm with
    | `Imm v -> 
        let shift x = bitv_of 2 |> shiftl b0 x in
        let s = word_as_bitv v in
        set Env.sp (add (var Env.sp) (shift s))
    | _ -> raise (Lift_Error "imm must be an immediate")

    let subspi imm = match imm with
    | `Imm v -> 
        let shift x = bitv_of 2 |> shiftl b0 x in
        let s = word_as_bitv v in
        set Env.sp (sub (var Env.sp) (shift s))
    | _ -> raise (Lift_Error "imm must be an immediate")

    let adc d s1 s2 = match d, s1, s2 with
    | `Reg d, `Reg s1, `Reg s2 -> 
        let (d, s1, s2) = (reg d, reg s1, reg s2) in
        seq 
            (set d (add (add (var s1) (var s2)) (bool_as_bitv (var Env.cf))))
            (Flags.set_adc (var s1) (var s2) d)
    | _ -> raise (Lift_Error "operands must be registers")

    let sbc d s = match d, s with
    | `Reg d, `Reg s -> 
        let (d, s) = (reg d, reg s) in
        seq
            (seq
                (set Env.tmp (var d))
                (set d (add (sub (var d) (var s)) (bool_as_bitv (var Env.cf))))
            )
            (Flags.set_sbc (var Env.tmp) (var s) d)
    | _ -> raise (Lift_Error "operands must be registers")

end