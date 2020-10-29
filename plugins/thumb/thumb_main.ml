let package = "bap"

open Base
open Bap_core_theory
open KB.Syntax

include Bap_main.Loggers()

module Defs = Thumb_defs
module Flags = Thumb_flags.Flags
module Insns = Thumb_insn
module Target = Arm_target

module MC = Bap.Std.Disasm_expert.Basic

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
    | `tLDRi, [dest; src; imm; _; _] ->
      lift_mem_single dest src ~src2:imm Ld W
    | `tLDRr, [dest; src1; src2; _; _] ->
      lift_mem_single dest src1 ~src2 Ld W
    | `tLDRpci, [dest; imm; _; _] ->
      lift_mem_single ~shift_val:0 dest (`Reg `PC) ~src2:imm Ld W
    | `tLDRspi, [dest; (`Reg `SP); imm; _; _] ->
      lift_mem_single dest (`Reg `SP) ~src2:imm Ld W
    | `tLDRBi, [dest; src; imm; _; _] ->
      lift_mem_single ~shift_val:0 dest src ~src2:imm Ld B
    | `tLDRBr, [dest; src1; src2; _; _] ->
      lift_mem_single dest src1 ~src2 Ld B
    | `tLDRHi, [dest; src; imm; _; _] ->
      lift_mem_single ~shift_val:1 dest src ~src2:imm Ld H
    | `tLDRHr, [dest; src1; src2; _; _] ->
      lift_mem_single dest src1 ~src2 Ld H
    | `tLDRSB, [dest; src1; src2; _; _] ->
      lift_mem_single dest src1 ~src2 Ld B ~sign:true
    | `tLDRSH, [dest; src1; src2; _; _] ->
      lift_mem_single dest src1 ~src2 Ld H ~sign:true
    | `tSTRi, [dest; src; imm; _; _] ->
      lift_mem_single dest src ~src2:imm St W
    | `tSTRr, [dest; src1; src2; _; _] ->
      lift_mem_single dest src1 ~src2 St W
    | `tSTRspi, [dest; (`Reg `SP); imm; _; _] ->
      lift_mem_single dest (`Reg `SP) ~src2:imm St W
    | `tSTRBi, [dest; src; imm; _; _] ->
      lift_mem_single ~shift_val:0 dest src ~src2:imm St B
    | `tSTRBr, [dest; src1; src2; _; _] ->
      lift_mem_single dest src1 ~src2 St B
    | `tSTRHi, [dest; src; imm; _; _] ->
      lift_mem_single ~shift_val:1 dest src ~src2:imm St H
    | `tSTRHr, [dest; src1; src2; _; _] ->
      lift_mem_single dest src1 ~src2 St H
    | `tSTMIA, dest :: _dest :: _ :: _nil_reg :: src_list (* looks like they should be different, but actually are the same in Thumb mode *)
    | `tSTMIA_UPD, dest :: _dest :: _ :: _nil_reg :: src_list ->
      Mem.store_multiple dest src_list |> move
    | `tLDMIA, dest :: _ :: _nil_reg :: src_list (* same as stmia *)
    | `tLDMIA_UPD, dest :: _ :: _nil_reg :: src_list ->
      Mem.load_multiple dest src_list |> move
    | `tPUSH, _ :: _nil_reg :: src_list ->
      Mem.push_multiple src_list |> move
    | `tPOP, _ :: _nil_reg :: src_list ->
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

  let lift_insn addr insn ops = match insn with
    | #move_insn -> lift_move_pre insn ops addr
    | #mem_insn -> lift_mem insn ops addr
    | #bits_insn -> lift_bits insn ops |> DSL.expand |> move
    | #branch_insn ->
      let ctrl_eff, data_eff = lift_branch insn ops addr in
      ctrl ctrl_eff data_eff addr (* var Env.pc *)

end

module Main = struct
  open Bap.Std

  let cmnz_opcode = Word.of_int 10 0x10b

  (* this is a temporary fix since bap-mc disassembler couldn't recognize CMN *)
  let fix_cmnz insn mem = match insn with
    | Some insn -> Some insn
    | None ->
      let opcode =
        let open Or_error.Monad_infix in
        let scale = Size.of_int_exn 16 in
        Memory.get ~scale mem >>=
        Word.extract ~hi:15 ~lo:6 in
      match opcode with
      | Ok opcode when Word.equal opcode cmnz_opcode ->
        Some `tCMNz
      | _ -> None

  let decode insn mem = match fix_cmnz (Insns.of_basic insn) mem with
    | None -> Or_error.errorf "Unknown instruction: %s"
                (MC.Insn.asm insn)
    | Some opcode -> match Insns.arm_ops (MC.Insn.ops insn) with
      | Ok ops -> Ok (opcode,ops)
      | Error _ as err -> err

  let (>>=?) x f = x >>= function
    | None -> KB.return Insn.empty
    | Some x -> f x

  let load () =
    KB.promise Theory.Semantics.slot @@ fun label ->
    KB.collect Theory.Label.encoding label >>= fun encoding ->
    if Theory.Language.equal Target.llvm_t32 encoding then
      KB.collect MC.Insn.slot label >>=? fun insn ->
      KB.collect Memory.slot label >>=? fun mem ->
      Theory.instance () >>= Theory.require >>= fun (module Core) ->
      let module Thumb = Thumb(Core) in
      let addr = Word.to_bitvec@@Memory.min_addr mem in
      match decode insn mem with
      | Ok (op,ops) -> Thumb.lift_insn addr op ops
      | Error err ->
        info "failed to decode a thumb instruction: %a"
          Error.pp err;
        KB.return Insn.empty
      | exception uncaught ->
        warning "failed to decode a thumb instruction: \
                 uncaught exception %s\nBacktrace:\n %s\n"
          (Exn.to_string uncaught)
          (Caml.Printexc.get_backtrace ());
        KB.return Insn.empty
    else KB.return Insn.empty
end

let () = Bap_main.Extension.declare @@ fun _ctxt ->
  Main.load ();
  Ok ()
