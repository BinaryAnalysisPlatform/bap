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

  let lift_move insn ops =
    let open Mov in
    match insn, ops with
    | `tADC, [|dest; src1; src2|] -> adc dest src1 src2
    | `tADDi3, [|dest; src; imm|] -> addi3 dest src imm
    | `tADDi8, [|dest; imm|] -> addi8 dest imm
    | `tADDrr, [|dest; src1; src2|] -> addrr dest src1 src2
    | `tADDhirr, [|dest; src|] -> addhirr dest src
    | `tADR, [|dest; imm|] -> adr dest imm
    | `tADDrSPi, [|dest; imm|] -> addrspi dest imm
    | `tADDspi, [|imm|] -> addspi imm
    | `tMOVr, [|dest; src|] -> mover dest src
    | `tMOVSr, [|dest; src|] -> movesr dest src
    | `tMOVi8, [|dest; imm|] -> movei8 dest imm
    | `tMUL, [|dest; src|] -> mul dest src
    | `tMVN, [|dest; src|] -> movenot dest src
    | `tSBC, [|dest; src|] -> sbc dest src
    | `tSUBi3, [|dest; src; imm|] -> subi3 dest src imm
    | `tSUBi8, [|dest; imm|] -> subi8 dest imm
    | `tSUBrr, [|dest; src1; src2|] -> subrr dest src1 src2
    | `tSUBspi, [|imm|] -> subspi imm
    | `tAND, [|dest; src|] -> andrr dest src
    | `tASRri, [|dest; src; imm|] -> asri dest src imm
    | `tASRrr, [|dest; src|] -> asrr dest src
    | `tBIC, [|dest; src|] -> bic dest src
    | `tCMNz, [|dest; src|] -> cmnz dest src
    | `tCMPi8, [|dest; imm|] -> cmpi8 dest imm
    | `tCMPr, [|dest; src|] -> cmpr dest src
    | `tCMPhir, [|dest; src|] -> cmphir dest src
    | `tEOR, [|dest; src|] -> eor dest src
    | `tLSLri, [|dest; src; imm|] -> lsli dest src imm
    | `tLSLrr, [|dest; src|] -> lslr dest src
    | `tLSRri, [|dest; src; imm|] -> lsri dest src imm
    | `tLSRrr, [|dest; src|] -> lsrr dest src
    | `tORR, [|dest; src|] -> orr dest src
    | `tRSB, [|dest; src; imm (* placeholder *)|] -> rsb dest src imm
    | `tREV, [|dest; src|] -> rev dest src
    | `tREV16, [|dest; src|] -> rev16 dest src
    | `tREVSH, [|dest; src|] -> revsh dest src
    | `tROR, [|dest; src|] -> ror dest src
    | `tTST, [|dest; src|] -> tst dest src
    | _ -> []

  let lift_mem_single ?(sign = false) dest src1 ?src2 (op : Defs.operation) (size : Defs.size) =
    match src2 with
    | Some src2 -> Mem.lift_mem_single ~sign dest src1 ~src2 op size |> move
    | None -> Mem.lift_mem_single ~sign dest src1 op size |> move

  let lift_mem insn ops addr =
    let open Defs in
    match insn, Array.to_list ops with
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
      Mem.store_multiple dest src_list |> move
    | `tLDMIA, dest :: src_list ->
      Mem.load_multiple dest src_list |> move
    | `tPUSH, src_list ->
      Mem.push_multiple src_list |> move
    | `tPOP, src_list ->
      let has_pc = List.exists src_list 
          (fun s -> match s with
             |`Reg `PC -> true
             | _ -> false) in
      let pop_eff = Mem.pop_multiple src_list in
      if has_pc then
        ctrl (jmp (var Env.pc)) pop_eff addr
      else move pop_eff
    | _ -> move pass

  let lift_bits insn ops =
    let open Bits in
    match insn, ops with
    | `tSXTB, [|dest; src|] -> sxtb dest src
    | `tSXTH, [|dest; src|] -> sxth dest src
    | `tUXTB, [|dest; src|] -> uxtb dest src
    | `tUXTH, [|dest; src|] -> uxth dest src
    | _ -> []

  (* these are not entirely complete *)
  let lift_branch insn ops addr =
    let open Defs in
    let open Branch in
    let addr = Core.int Env.value addr in
    match insn, ops with
    | `tBcc, [|cond; target|] -> tbcc cond target addr
    | `tB, [|target|] -> tb target addr
    | `tBL, [|target|] -> tbl target
    | `tBLXi, [|target|] -> tblxi target
    | `tBLXr, [|target|] -> tblxr target
    | `tBX, [|target|] -> tbx target
    | _ -> (skip, pass)

  let lift_with (addr : Bitvec.t) (insn : Thumb_defs.insn)
      (ops : Thumb_defs.op array) = match insn with
    | #move_insn -> lift_move insn ops |> DSL.expand |> move
    | #mem_insn -> lift_mem insn ops addr
    | #bits_insn -> lift_bits insn ops |> DSL.expand |> move
    | #branch_insn -> 
      let ctrl_eff, data_eff = lift_branch insn ops addr in
      ctrl ctrl_eff data_eff addr (* var Env.pc *)

end

open Bap.Std

let run_lifter _label addr insn _mem 
    (lifter : Bitvec.t -> Defs.insn -> Defs.op array -> unit Theory.eff) =
  match Insns.of_basic insn with
  | None -> raise (Defs.Lift_Error "unknown instruction")
  | Some arm_insn -> 
    match Insns.arm_ops (Disasm_expert.Basic.Insn.ops insn) with
    | Error err -> raise (Defs.Lift_Error (Error.to_string_hum err))
    | Ok ops -> lifter addr arm_insn ops

let () =
  KB.promise Theory.Program.Semantics.slot @@ fun label ->
  Theory.instance () >>= Theory.require >>= fun (module Core) ->
  KB.collect Disasm_expert.Basic.Insn.slot label >>= fun insn -> (* the LLVM provided decoding *)
  KB.collect Memory.slot label >>= fun mem -> (* the memory chunk, probably not needed *)
  let module Lifter = Thumb(Core) in
  match insn, mem with
  | Some insn, Some mem ->
    let addr = Word.to_bitvec@@Memory.min_addr mem in
    run_lifter label addr insn mem Lifter.lift_with
  | _ -> KB.return Insn.empty