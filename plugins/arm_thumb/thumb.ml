open Bap_core_theory
open Base
open KB.Syntax

module Defs = Thumb_defs
module Flags = Thumb_flags.Flags
module Insns = Thumb_insn

let package = "arm-thumb"

type insns = Defs.insn * (Defs.op list)

module Thumb(Core : Theory.Core) = struct
  open Core
  open Defs
  module Env = Thumb_env.Env

  module Mov = Thumb_mov.Mov(Core)
  module Mem = Thumb_mem.Mem(Core)
  module Bits = Thumb_bits.Bits(Core)
  module Branch = Thumb_branch.Branch(Core)
  module Utils = Thumb_util.Utils(Core)
  module DSL = Thumb_dsl.Make(Core)

  open Utils

  let move eff =
    KB.Object.create Theory.Program.cls >>= fun lbl ->
    blk lbl eff skip

  let ctrl eff data pc = 
    Theory.Label.for_addr pc >>= fun lbl ->
    blk lbl data eff

  let lift_move insn ops addr =
    let open Mov in
    match insn, ops with
    | `tADC, [|dest; _cpsr; _dest; src; _unknown; _|] -> adc dest src
    | `tADDi3, [|dest; _cpsr; src; imm; _unknown; _|] -> addi3 dest src imm
    | `tADDi8, [|dest; _cpsr; _dest; imm; _unknown; _|] -> addi8 dest imm
    | `tADDrr, [|dest; _cpsr; src1; src2; _unknown; _|] -> addrr dest src1 src2
    | `tADDhirr, [|dest; _dest; src; _unknown; _|] -> addhirr dest src
    | `tADR, [|dest; imm; _unknown; _|] -> adr dest imm addr
    | `tADDrSPi, [|dest; `Reg `SP; imm; _unknown; _|] -> addrspi dest imm
    | `tADDspi, [|`Reg `SP; _sp; imm; _unknown; _|] -> addspi imm
    | `tMOVr, [|dest; src; _unknown; _|] -> mover dest src
    | `tMOVSr, [|dest; _cpsr; src; _unknown; _|] -> movesr dest src (* this has been properly encoded by `tADDi3 dest #0 src` *)
    | `tMOVi8, [|dest; _cpsr; imm; _unknown; _|] -> movei8 dest imm
    | `tMUL, [|dest; _cpsr; src; _dest; _unknown; _|] -> mul dest src
    | `tMVN, [|dest; _cpsr; src; _unknown; _|] -> movenot dest src
    | `tSBC, [|dest; _cpsr; _dest; src; _unknown; _|] -> sbc dest src
    | `tSUBi3, [|dest; _cpsr; src; imm; _unknown; _|] -> subi3 dest src imm
    | `tSUBi8, [|dest; _cpsr; _dest; imm; _unknown; _|] -> subi8 dest imm
    | `tSUBrr, [|dest; _cpsr; src1; src2; _unknown; _|] -> subrr dest src1 src2
    | `tSUBspi, [|`Reg `SP; _sp; imm; _unknown; _|] -> subspi imm
    | `tAND, [|dest; _cpsr; _dest; src; _unknown; _|] -> andrr dest src
    | `tASRri, [|dest; _cpsr; src; imm; _unknown; _|] -> asri dest src imm
    | `tASRrr, [|dest; _cpsr; _dest; src; _unknown; _|] -> asrr dest src
    | `tBIC, [|dest; _cpsr; _dest; src; _unknown; _|] -> bic dest src
    | `tCMNz, [|dest; src; _unknown; _|] -> cmnz dest src (* TODO : we've got an error here *)
    | `tCMPi8, [|dest; imm; _unknown; _|] -> cmpi8 dest imm
    | `tCMPr, [|dest; src; _unknown; _|] -> cmpr dest src
    | `tEOR, [|dest; _cpsr; _dest; src; _unknown; _|] -> eor dest src
    | `tLSLri, [|dest; _cpsr; src; imm; _unknown; _|] -> lsli dest src imm
    | `tLSLrr, [|dest; _cpsr; _dest; src; _unknown; _|] -> lslr dest src
    | `tLSRri, [|dest; _cpsr; src; imm; _unknown; _|] -> lsri dest src imm
    | `tLSRrr, [|dest; _cpsr; _dest; src; _unknown; _|] -> lsrr dest src
    | `tORR, [|dest; _cpsr; _dest; src; _unknown; _|] -> orr dest src
    | `tRSB, [|dest; _cpsr; src; _unknown; _ (* placeholder *)|] -> rsb dest src (`Imm (Bap.Std.Word.zero 32))
    | `tREV, [|dest; src; _unknown; _|] -> rev dest src
    | `tREV16, [|dest; src; _unknown; _|] -> rev16 dest src
    | `tREVSH, [|dest; src; _unknown; _|] -> revsh dest src
    | `tROR, [|dest; _cpsr; _dest; src; _unknown; _|] -> ror dest src
    | `tTST, [|dest; src; _unknown; _|] -> tst dest src
    | _ -> []

  let lift_move_pre insn ops addr =
    let open Mov in
    let addr_bitv = Core.int Env.value addr in
    let filter_pc = function
      | `Reg `PC -> addr_bitv
      | src -> DSL.(!$src) in
    match insn, ops with (* resolve the PC-involved instructions here *)
    | `tMOVr, [|`Reg `PC; src; _unknown; _|] -> 
      ctrl DSL.(jmp !$+src) pass addr
    | `tMOVr, [|dest; `Reg `PC; _unknown; _|] ->
      move DSL.(!$$+dest := addr_bitv)
    | `tADDhirr, [|`Reg `PC; _dest; src; _unknown; _|] ->
      let src = filter_pc src in
      ctrl DSL.(jmp (src + addr_bitv)) pass addr
    | `tADDhirr, [|dest; _dest; `Reg `PC; _unknown; _|] ->
      move DSL.(!$$+dest := !$+dest + addr_bitv)
    | `tCMPhir, [|dest; src; _unknown; _|] -> 
      let src = filter_pc src in
      let dest = filter_pc dest in
      cmphir dest src |> DSL.expand |> move
    | _, _ -> lift_move insn ops addr_bitv |> DSL.expand |> move

  let lift_mem_single ?(sign = false) ?(shift_val = 2) dest src1 ?src2 (op : Defs.operation) (size : Defs.size) =
    match src2 with
    | Some src2 -> Mem.lift_mem_single ~sign ~shift_val dest src1 ~src2 op size |> move
    | None -> Mem.lift_mem_single ~sign ~shift_val dest src1 op size |> move

  let lift_mem insn ops addr =
    let open Defs in
    match insn, Array.to_list ops with
    | `tLDRi, [dest; src; imm; unknown; _] -> 
      lift_mem_single dest src ~src2:imm Ld W
    | `tLDRr, [dest; src1; src2; unknown; _] -> 
      lift_mem_single dest src1 ~src2 Ld W
    | `tLDRpci, [dest; imm; unknown; _] -> 
      lift_mem_single ~shift_val:0 dest (`Reg `PC) ~src2:imm Ld W
    | `tLDRspi, [dest; (`Reg `SP); imm; unknown; _] -> 
      lift_mem_single dest (`Reg `SP) ~src2:imm Ld W
    | `tLDRBi, [dest; src; imm; unknown; _] -> 
      lift_mem_single ~shift_val:0 dest src ~src2:imm Ld B
    | `tLDRBr, [dest; src1; src2; unknown; _] -> 
      lift_mem_single dest src1 ~src2 Ld B
    | `tLDRHi, [dest; src; imm; unknown; _] -> 
      lift_mem_single ~shift_val:1 dest src ~src2:imm Ld H
    | `tLDRHr, [dest; src1; src2; unknown; _] -> 
      lift_mem_single dest src1 ~src2 Ld H
    | `tLDRSB, [dest; src1; src2; unknown; _] -> 
      lift_mem_single dest src1 ~src2 Ld B ~sign:true
    | `tLDRSH, [dest; src1; src2; unknown; _] -> 
      lift_mem_single dest src1 ~src2 Ld H ~sign:true
    | `tSTRi, [dest; src; imm; unknown; _] -> 
      lift_mem_single dest src ~src2:imm St W
    | `tSTRr, [dest; src1; src2; unknown; _] -> 
      lift_mem_single dest src1 ~src2 St W
    | `tSTRspi, [dest; (`Reg `SP); imm; unknown; _] -> 
      lift_mem_single dest (`Reg `SP) ~src2:imm St W
    | `tSTRBi, [dest; src; imm; unknown; _] -> 
      lift_mem_single ~shift_val:0 dest src ~src2:imm St B
    | `tSTRBr, [dest; src1; src2; unknown; _] -> 
      lift_mem_single dest src1 ~src2 St B
    | `tSTRHi, [dest; src; imm; unknown; _] -> 
      lift_mem_single ~shift_val:1 dest src ~src2:imm St H
    | `tSTRHr, [dest; src1; src2; unknown; _] -> 
      lift_mem_single dest src1 ~src2 St H
    | `tSTMIA, dest :: _dest :: _unknown :: _nil_reg :: src_list (* looks like they should be different, but actually are the same in Thumb mode *)
    | `tSTMIA_UPD, dest :: _dest :: _unknown :: _nil_reg :: src_list ->
      Mem.store_multiple dest src_list |> move
    | `tLDMIA, dest :: _unknown :: _nil_reg :: src_list (* same as stmia *)
    | `tLDMIA_UPD, dest :: _unknown :: _nil_reg :: src_list ->
      Mem.load_multiple dest src_list |> move
    | `tPUSH, _unknown :: _nil_reg :: src_list ->
      Mem.push_multiple src_list |> move
    | `tPOP, _unknown :: _nil_reg :: src_list ->
      Theory.Var.fresh Env.value >>= fun pc ->
      let has_pc = List.exists src_list 
          (fun s -> match s with
             |`Reg `PC -> true
             | _ -> false) in
      let pop_eff = Mem.pop_multiple src_list pc in
      if has_pc then
        ctrl (jmp (var pc)) pop_eff addr
      else move pop_eff
    | _ -> move pass

  let lift_bits insn ops =
    let open Bits in
    match insn, ops with
    | `tSXTB, [|dest; src; _unknown; _|] -> sxtb dest src
    | `tSXTH, [|dest; src; _unknown; _|] -> sxth dest src
    | `tUXTB, [|dest; src; _unknown; _|] -> uxtb dest src
    | `tUXTH, [|dest; src; _unknown; _|] -> uxth dest src
    | _ -> []

  (* these are not entirely complete *)
  let lift_branch insn ops addr =
    let open Defs in
    let open Branch in
    let addr = Core.int Env.value addr in
    match insn, ops with
    | `tBcc, [|target; cond; _cpsr|] -> tbcc cond target addr
    | `tB, [|target; _unknown; _|] -> tb target addr
    | `tBL, [|_unknown; _nil_reg; target; _cpsr|] -> tbl target addr 
    | `tBLXi, [|_unknown; _nil_reg; target; _cpsr|] -> tblxi target addr
    | `tBLXr, [|_unknown; _nil_reg; target|] -> tblxr target addr
    | `tBX, [|target; _unknown; _|] -> tbx target
    | _ -> (skip, pass)

  let lift_with (addr : Bitvec.t) (insn : Thumb_defs.insn)
      (ops : Thumb_defs.op array) =
    match insn with
    | #move_insn -> lift_move_pre insn ops addr
    | #mem_insn -> lift_mem insn ops addr
    | #bits_insn -> lift_bits insn ops |> DSL.expand |> move
    | #branch_insn -> 
      let ctrl_eff, data_eff = lift_branch insn ops addr in
      ctrl ctrl_eff data_eff addr (* var Env.pc *)

end

open Bap.Std

(* this is a temporary fix since bap-mc disassembler couldn't recognize CMN *)
let fix_cmnz insn mem = match insn with
  | None -> if Memory.length mem = 2 then
      let insn_word = Or_error.(ok (Size.of_int 16 >>= fun hw ->
                                    Memory.get ~scale:hw mem >>= Word.extract ~hi:15 ~lo:6)) in
      Option.(insn_word >>= fun insn -> 
              if Word.equal (Word.of_int 10 0x10b) insn 
              then Some `tCMNz 
              else None)
    else None
  | Some insn -> Some insn

let run_lifter _label addr insn mem 
    (lifter : Bitvec.t -> Defs.insn -> Defs.op array -> unit Theory.eff) =
  match fix_cmnz (Insns.of_basic insn) mem with
  | None -> raise (Defs.Lift_Error "unknown instruction")
  | Some arm_insn -> 
    match Insns.arm_ops (Disasm_expert.Basic.Insn.ops insn) with
    | Error err -> raise (Defs.Lift_Error (Error.to_string_hum err))
    | Ok ops -> lifter addr arm_insn ops

let () =
  KB.promise Theory.Program.Semantics.slot @@ fun label ->
  Theory.instance () >>= Theory.require >>= fun (module Core) ->
  KB.collect Arch.slot label >>= fun arch ->
  KB.collect Disasm_expert.Basic.Insn.slot label >>= fun insn -> (* the LLVM provided decoding *)
  KB.collect Memory.slot label >>= fun mem -> (* the memory chunk, probably not needed *)
  let module Lifter = Thumb(Core) in
  match arch, insn, mem with
  | #Arch.thumbeb, Some insn, Some mem
  | #Arch.thumb, Some insn, Some mem ->
    let addr = Word.to_bitvec@@Memory.min_addr mem in
    run_lifter label addr insn mem Lifter.lift_with
  | _ -> KB.return Insn.empty