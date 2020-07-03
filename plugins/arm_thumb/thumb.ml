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
  module Bits = Thumb_bits.Bits(Core)
  module Utils = Thumb_util.Utils(Core)

  open Utils

  let move eff =
    KB.Object.create Theory.Program.cls >>= fun lbl ->
    blk lbl eff skip

  let ctrl eff data pc = 
    Theory.Label.for_addr pc >>= fun lbl ->
    blk lbl data eff

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
    | `tAND, [dest; src] -> andrr dest src
    | `tASRri, [dest; src; imm] -> asri dest src imm
    | `tASRrr, [dest; src] -> asrr dest src
    | `tBIC, [dest; src] -> bic dest src
    | `tCMNz, [dest; src] -> cmnz dest src
    | `tCMPi8, [dest; imm] -> cmpi8 dest imm
    | `tCMPr, [dest; src] -> cmpr dest src
    | `tCMPhir, [dest; src] -> cmphir dest src
    | `tEOR, [dest; src] -> eor dest src
    | `tLSLri, [dest; src; imm] -> lsli dest src imm
    | `tLSLrr, [dest; src] -> lslr dest src
    | `tLSRri, [dest; src; imm] -> lsri dest src imm
    | `tLSRrr, [dest; src] -> lsrr dest src
    | `tORR, [dest; src] -> orr dest src
    | `tRSB, [dest; src; imm (* placeholder *)] -> rsb dest src imm
    | `tREV, [dest; src] -> rev dest src
    | `tREV16, [dest; src] -> rev16 dest src
    | `tREVSH, [dest; src] -> revsh dest src
    | `tROR, [dest; src] -> ror dest src
    | `tTST, [dest; src] -> tst dest src
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
    | `tSTMIA, dest :: src_list ->
      store_multiple dest src_list
    | `tLDMIA, dest :: src_list ->
      load_multiple dest src_list
    | `tPUSH, src_list ->
      push_multiple src_list
    | `tPOP, src_list ->
      pop_multiple src_list (* TODO: PC might have been changed *)
    | _ -> pass

  let lift_bits insn ops =
    let open Bits in
    match insn, ops with
    | `tSXTB, [dest; src] -> sxtb dest src
    | `tSXTH, [dest; src] -> sxth dest src
    | `tUXTB, [dest; src] -> uxtb dest src
    | `tUXTH, [dest; src] -> uxth dest src
    | _ -> pass

  let bcc cond target = match cond, target with
    | `Imm cond, `Imm target -> 
      let z = var Env.zf in
      let c = var Env.cf in
      let v = var Env.vf in
      let n = var Env.nf in
      let eq_ a b = or_ (and_ a b) (and_ (inv a) (inv b)) in
      let cond_val = match Bap.Std.Word.to_int cond |> Result.ok |> Option.value_exn |> Defs.of_int_exn  with
        | `EQ -> z
        | `NE -> inv z
        | `CS -> c
        | `CC -> inv c
        | `MI -> n
        | `PL -> inv n
        | `VS -> v
        | `VC -> inv v
        | `HI -> and_ c (inv z)
        | `LS -> or_ (inv c) z
        | `GE -> eq_ n v
        | `LT -> eq_ n v |> inv
        | `GT -> and_ (inv z) (eq_ n v)
        | `LE -> or_ z (eq_ n v |> inv)
        | `AL -> b1 
      in let jump_address = add (var Env.pc) (lshift (word_as_bitv target) (bitv_of 1)) in
      branch cond_val
        (
          jmp jump_address
        )
        (skip)
    | _ -> raise @@ Lift_Error "operands must be immediate"

  let lift_b ?(link = false) ?(state = false) ?(shl = true) ?offset base =
    let open Defs in
    let address = match base, offset with
      | `Reg r, Some `Imm v -> let r = reg r in
        add (var r) ((if shl then Bap.Std.Word.(lshift v (of_int 32 1)) else v) |> word_as_bitv)
      | `Reg r, None -> let r = reg r in
        if shl then shiftl b0 (var r) (bitv_of 2) else var r
      | _ -> raise (Lift_Error "invalid operands")
    in jmp address

  (* these are not entirely complete *)
  let lift_branch insn ops =
    let open Defs in
    match insn, ops with
    | `tBcc, [cond; target] -> bcc cond target
    | `tB, [target] -> lift_b (`Reg `PC) ~offset:target
    | `tBL, [target] -> lift_b (`Reg `PC) ~offset:target ~shl:false ~link:true
    | `tBLXi, [target] -> lift_b (`Reg `PC) ~offset:target ~shl:false ~link:true ~state:true
    | `tBLXr, [target] -> lift_b target ~shl:false ~link:true ~state:true
    | `tBX, [target] -> lift_b target ~state:true
    | _ -> skip

  let lift ((ins, ops) : insns) : unit Theory.Effect.t KB.t = 
    match ins with
    | #move_insn -> lift_move ins ops |> move
    | #mem_insn -> lift_mem ins ops |> move
    | #bits_insn -> lift_bits ins ops |> move
    (* this is malformed for now *)
    | #branch_insn -> ctrl (lift_branch ins ops) pass (Bitvec.zero) (* var Env.pc *)

end
