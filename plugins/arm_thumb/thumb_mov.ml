open Bap_core_theory
open Base
open KB.Syntax

module Env  = Thumb_env.Env
module Flags = Thumb_flags.Flags

exception Lift_Error = Thumb_defs.Lift_Error

module Mov(Core : Theory.Core) = struct
open Core

    module Utils = Thumb_util.Utils(Core)
    module Flags = Flags(Core)

    open Utils

    let rseq x y = seq y x

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

    let andrr dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        seq (set d (logand (var d) (var s))) (Flags.set_nzf d) 
    | _ -> raise (Lift_Error "dest or src is not a register")

    let asri dest src imm = match dest, src, imm with
    | `Reg d, `Reg s, `Imm v -> 
        let nth_bit n e = cast Env.bit_val b0 (shiftr b0 e n) |> msb in
        let (d, s1, s2) = (reg d, reg s, word_as_bitv v) in
        (if Bap.Std.Word.is_zero v then
            seq
                (set Env.cf (var s1 |> msb))
                (set d (ite (var s1 |> msb) (bitv_of 0xffffffff) (bitv_of 0)))
        else let take_n = Bap.Std.Word.sub v (Bap.Std.Word.of_int 32 1) |> word_as_bitv in
            seq
                (set Env.cf (nth_bit take_n (var s1)))
                (set d (arshift (var s1) s2))) 
        |> rseq (Flags.set_nzf d)
    | _ -> raise (Lift_Error "incorrect operands")

    (* ASRr has a rather complex logic, see A7.1.12 *)
    let asrr dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        let nth_bit n e = cast Env.bit_val b0 (shiftr b0 e n) |> msb in
        let seg = cast Env.byte b0 (var s) in
         (seq
            (set Env.cf (ite (ult seg (bitv_of 32 |> cast Env.byte b0))
                            (ite (eq seg (zero Env.byte))
                                (var Env.cf)
                                (nth_bit (sub seg (bitv_of 1 |> cast Env.byte b0)) (var d))
                            )
                            (msb (var d))
                        )
            )
            (set d (ite (ult seg (bitv_of 32 |> cast Env.byte b0))
                            (ite (eq seg (zero Env.byte))
                                (var d)
                                (arshift (var d) seg)
                            )
                            (ite (msb (var d))
                                (bitv_of 0xffffffff)
                                (bitv_of 0)
                            )
                        )
            )) |> rseq (Flags.set_nzf d)
    | _ -> raise (Lift_Error "dest or src is not a register")

    let bic dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        seq
            (set d (logand (var d) (not (var s))))
            (Flags.set_nzf d)
    | _ -> raise (Lift_Error "dest or src is not a register")

    let cmnz dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        seq
            (set Env.tmp (add (var d) (var s)))
            (Flags.set_add (var d) (var s) Env.tmp)
    | _ -> raise (Lift_Error "dest or src is not a register")

    let cmpi8 dest imm = match dest, imm with
    | `Reg d, `Imm v -> 
        let (d, s) = (reg d, word_as_bitv v) in
        seq
            (set Env.tmp (sub (var d) s)) 
            (Flags.set_sub (var d) s Env.tmp) 
    | _ -> raise (Lift_Error "incorrect operands")

    let cmpr dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        seq
            (set Env.tmp (sub (var d) (var s)))
            (Flags.set_sub (var d) (var s) Env.tmp)
    | _ -> raise (Lift_Error "dest or src is not a register")

    let cmphir dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg_wide d, reg_wide s in
        seq
            (set Env.tmp (sub (var d) (var s)))
            (Flags.set_sub (var d) (var s) Env.tmp)
    | _ -> raise (Lift_Error "dest or src is not a register")

    let eor dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        seq
            (set d (logxor (var d) (var s)))
            (Flags.set_nzf d)
    | _ -> raise (Lift_Error "dest or src is not a register")

    let lsli dest src imm = match dest, src, imm with
    | `Reg d, `Reg s, `Imm v -> 
        let nth_bit n e = cast Env.bit_val b0 (shiftr b0 e n) |> msb in
        let (d, s1, s2) = (reg d, reg s, word_as_bitv v) in
        (if Bap.Std.Word.is_zero v then
            set d (var s1)
        else let take_n = Bap.Std.Word.sub (Bap.Std.Word.of_int 32 32) v |> word_as_bitv in
            seq
                (set Env.cf (nth_bit take_n (var s1)))
                (set d (shiftl b0 (var s1) s2))) 
        |> rseq (Flags.set_nzf d)
    | _ -> raise (Lift_Error "incorrect operands")

    (* LSLr has a rather complex logic, see A7.1.39 *)
    let lslr dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        let nth_bit n e = cast Env.bit_val b0 (shiftr b0 e n) |> msb in
        let seg = cast Env.byte b0 (var s) in
         (seq
            (set Env.cf (ite (ult seg (bitv_of 32 |> cast Env.byte b0))
                            (ite (eq seg (zero Env.byte))
                                (var Env.cf)
                                (nth_bit (sub (bitv_of 32 |> cast Env.byte b0) seg) (var d))
                            )
                            (ite (eq seg (bitv_of 32 |> cast Env.byte b0))
                                (lsb (var d))
                                b0
                            )
                        )
            )
            (set d (ite (ult seg (bitv_of 32 |> cast Env.byte b0))
                            (ite (eq seg (zero Env.byte))
                                (var d)
                                (shiftl b0 (var d) seg)
                            )
                            (zero Env.value)
                        )
            )) |> rseq (Flags.set_nzf d)
    | _ -> raise (Lift_Error "dest or src is not a register")

    let lsri dest src imm = match dest, src, imm with
    | `Reg d, `Reg s, `Imm v -> 
        let nth_bit n e = cast Env.bit_val b0 (shiftr b0 e n) |> msb in
        let (d, s1, s2) = (reg d, reg s, word_as_bitv v) in
        (if Bap.Std.Word.is_zero v then
            seq
                (set Env.cf (var s1 |> msb))
                (set d (bitv_of 0))
        else let take_n = Bap.Std.Word.sub v (Bap.Std.Word.of_int 32 1) |> word_as_bitv in
            seq
                (set Env.cf (nth_bit take_n (var s1)))
                (set d (shiftr b0 (var s1) s2))) 
        |> rseq (Flags.set_nzf d)
    | _ -> raise (Lift_Error "incorrect operands")

    let lsrr dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        let nth_bit n e = cast Env.bit_val b0 (shiftr b0 e n) |> msb in
        let seg = cast Env.byte b0 (var s) in
         (seq
            (set Env.cf (ite (ult seg (bitv_of 32 |> cast Env.byte b0))
                            (ite (eq seg (zero Env.byte))
                                (var Env.cf)
                                (nth_bit (sub seg (bitv_of 1 |> cast Env.byte b0)) (var d))
                            )
                            (ite (eq seg (bitv_of 32 |> cast Env.byte b0))
                                (msb (var d))
                                b0
                            )
                        )
            )
            (set d (ite (ult seg (bitv_of 32 |> cast Env.byte b0))
                            (ite (eq seg (zero Env.byte))
                                (var d)
                                (shiftr b0 (var d) seg)
                            )
                            (zero Env.value)
                        )
            )) |> rseq (Flags.set_nzf d)
    | _ -> raise (Lift_Error "dest or src is not a register")

    let orr dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        seq (set d (logor (var d) (var s))) (Flags.set_nzf d) 
    | _ -> raise (Lift_Error "dest or src is not a register")

    (* This is actually code named `neg Rd Rm`, but llvm encodes it as `rsb Rd Rm #0` *)
    let rsb dest src imm = match dest, src, imm with
    | `Reg d, `Reg s, `Imm v -> 
        let (d, s1, s2) = (reg d, reg s, word_as_bitv v) in
        seq 
            (set d (sub s2 (var s1))) 
            (Flags.set_sub s2 (var s1) d) 
    | _ -> raise (Lift_Error "incorrect operands")

    let rev dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        let s = var s in
        let i24 = bitv_of 24 in
        let i8 = bitv_of 8 in
        let umask = bitv_of 0xff0000 in
        let lmask = bitv_of 0xff00 in
        let rev =
            logor (lshift s i24)
            (logor (rshift s i24)
                (logor
                    (rshift (logand s umask) i8)
                    (lshift (logand s lmask) i8)
                )
            )
        in set d rev
    | _ -> raise (Lift_Error "dest or src is not a register")

    let rev_halfword hf =
        let i8 = bitv_of 8 in
            logor (lshift hf i8) (rshift hf i8)

    let rev16 dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        let s = var s in
        let rev_extend v = 
            cast Env.half_word b0 v |> rev_halfword |> cast Env.value b0 in
        let i16 = bitv_of 16 in
        let rev = logor 
                (rev_extend s) 
                (rshift s i16 |> rev_extend |> lshift i16)
        in set d rev
    | _ -> raise (Lift_Error "dest or src is not a register")

    let revsh dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        let s = var s in
        let rev_extend v = 
            cast Env.half_word b0 v |> rev_halfword |> cast Env.value b0 in
        let rev = logand (rev_extend s) 
                    (ite (cast Env.byte b0 s |> msb)
                        (bitv_of 0xffff0000)
                        (bitv_of 0)
                    )
        in set d rev
    | _ -> raise (Lift_Error "dest or src is not a register")

    let ror dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        let nth_bit n e = cast Env.bit_val b0 (shiftr b0 e n) |> msb in
        let seg = cast Env.byte b0 (var s) in
        let seg4 = cast Env.half_byte b0 (var s) in
         (seq
            (set Env.cf (ite (eq seg (zero Env.byte))
                            (var Env.cf)
                            (ite (eq seg4 (zero Env.half_byte))
                                (msb (var d))
                                (nth_bit (sub seg4 (bitv_of 1 |> cast Env.half_byte b0)) (var d))
                            )
                        )
            )
            (set d (ite (eq seg4 (zero Env.half_byte))
                        (var d)
                        (
                            let seg4_comp = sub (bitv_of 32 |> cast Env.half_byte b0) seg4 in
                                logand (rshift (var d) seg4) (lshift (var d) seg4_comp)
                        )
                    )
            )) |> rseq (Flags.set_nzf d)
    | _ -> raise (Lift_Error "dest or src is not a register")

    let tst dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        seq
            (set Env.tmp (logand (var d) (var s)))
            (Flags.set_nzf Env.tmp)
    | _ -> raise (Lift_Error "dest or src is not a register")

end