let package = "bap"

open Core_kernel
open Bap_core_theory
open Bap.Std
open KB.Syntax

include Bap_main.Loggers()

open Thumb_opcodes
open Thumb_core

module MC = Bap.Std.Disasm_expert.Basic
module Target = Arm_target

type insns = opcode * Op.t array

let string_of_opcode x = Sexp.to_string (sexp_of_opcode x)
let string_of_operands ops =
  Array.map ops ~f:Op.to_string |>
  String.concat_array ~sep:", "

let pp_insn ppf (op,ops) =
  Format.fprintf ppf "%a(%s)"
    Sexp.pp (sexp_of_opcode op)
    (string_of_operands ops)

module Thumb(CT : Theory.Core) = struct

  let reg r = Theory.Var.define s32 (Reg.name r)
  let imm x = Option.value_exn (Imm.to_int x)
  let regs rs = List.map rs ~f:(function
      | Op.Reg r -> reg r
      | _ -> failwith "invalid multireg instruction")

  let cnd x = match Option.value_exn (Imm.to_int x) with
    | 0 ->  `EQ
    | 1 ->  `NE
    | 2 ->  `CS
    | 3 ->  `CC
    | 4 ->  `MI
    | 5 ->  `PL
    | 6 ->  `VS
    | 7 ->  `VC
    | 8 ->  `HI
    | 9 ->  `LS
    | 10 -> `GE
    | 11 -> `LT
    | 12 -> `GT
    | 13 -> `LE
    | 14 -> `AL
    | _ -> failwith "expected a condition code"


  let is_pc v = Theory.Var.name v = "PC"
  let has_pc = List.exists ~f:is_pc
  let remove_pc = List.filter ~f:(Fn.non is_pc)

  let lift_move _addr opcode insn =
    let module T = Thumb_mov.Make(CT) in
    let open T in
    match opcode, (MC.Insn.ops insn : Op.t array) with
    | `tADDi3, [| Reg rd; _; Reg rn; Imm x; _; _|] ->
      addi3 (reg rd) (reg rn) (imm x)
    | `tADDi8, [|Reg rd; _; _ ; Imm x; _; _|] ->
      addi8 (reg rd) (imm x)
    | `tADDrr, [|Reg rd; _; Reg rn; Reg rm; _; _|] ->
      addrr (reg rd) (reg rn) (reg rm)
    | `tADDrSPi, [|Reg rd; _; Imm x; _; _;|] ->
      addrspi (reg rd) (imm x * 4)
    | `tADDspi, [|_; _; Imm x; _; _|] ->
      addspi (imm x * 4)
    | `tSUBi3, [| Reg rd; _; Reg rn; Imm x; _; _|] ->
      subi3 (reg rd) (reg rn) (imm x)
    | `tSUBi8, [|Reg rd; _; _ ; Imm x; _; _|] ->
      subi8 (reg rd) (imm x)
    | `tSUBrr, [|Reg rd; _; Reg rn; Reg rm; _; _|] ->
      subrr (reg rd) (reg rn) (reg rm)
    | `tSUBspi, [|_; _; Imm x; _; _|] ->
      subspi (imm x * 4)
    | `tMOVi8, [|Reg rd; _; Imm x; _; _|] ->
      movi8 (reg rd) (imm x)
    | `tMOVSr, [|Reg rd; Reg rn|] ->
      movsr (reg rd) (reg rn)
    | `tMOVr, [|Reg rd; Reg rn; _; _|] ->
      movr (reg rd) (reg rn)
    | `tASRri, [|Reg rd; _; Reg rm; Imm x; _; _|] ->
      asri (reg rd) (reg rm) (imm x)
    | `tLSRri, [|Reg rd; _; Reg rm; Imm x; _; _|] ->
      lsri (reg rd) (reg rm) (imm x)
    | `tLSLri, [|Reg rd; _; Reg rm; Imm x; _; _|] ->
      lsli (reg rd) (reg rm) (imm x)
    | `tCMPi8, [|Reg rn; Imm x;_;_|] ->
      cmpi8 (reg rn) (imm x)
    | `tCMPr, [|Reg rn; Reg rm;_;_|] ->
      cmpr (reg rn) (reg rm)
    | `tORR, [|Reg rd; _; _; Reg rm; _; _|] ->
      lorr (reg rd) (reg rm)
    | insn ->
      info "unhandled move instruction: %a" pp_insn insn;
      !!Insn.empty


  let lift_mem pc opcode insn =
    let module Mem = Thumb_mem.Make(CT) in
    let open Mem in
    match opcode, (MC.Insn.ops insn : Op.t array) with
    | `tLDRi,   [|Reg rd; Reg rm; Imm i; _; _|]
    | `tLDRspi, [|Reg rd; Reg rm; Imm i; _; _|] ->
      ldri (reg rd) (reg rm) (imm i * 4)
    | `tLDRr, [|Reg rd; Reg rm; Reg rn; _; _|]
    | `t2LDRs, [|Reg rd; Reg rm; Reg rn; _; _; _|] ->
      ldrr (reg rd) (reg rm) (reg rn)
    | `tLDRpci, [|Reg rd; Imm i; _; _|] ->
      ldrpci (reg rd) W32.(pc + int 2) (imm i)
    | `t2LDRpci, [|Reg rd; Imm i; _; _|] ->
      ldrpci (reg rd) W32.(pc + int 4) (imm i)
    | `tLDRBi, [|Reg rd; Reg rm; Imm i; _; _|] ->
      ldrbi (reg rd) (reg rm) (imm i)
    | `tLDRBr, [|Reg rd; Reg rm; Reg rn; _; _|] ->
      ldrbr (reg rd) (reg rm) (reg rn)
    | `tLDRSB, [|Reg rd; Reg rm; Reg rn; _; _|] ->
      ldrsb (reg rd) (reg rm) (reg rn)
    | `tLDRHi, [|Reg rd; Reg rm; Imm i; _; _|] ->
      ldrhi (reg rd) (reg rm) (imm i * 2)
    | `tLDRHr, [|Reg rd; Reg rm; Reg rn; _; _|] ->
      ldrhr (reg rd) (reg rm) (reg rn)
    | `tLDRSH, [|Reg rd; Reg rm; Reg rn; _; _|] ->
      ldrsh (reg rd) (reg rm) (reg rn)
    | `tSTRspi, [|Reg rd; Reg rm; Imm i; _; _|]
    | `tSTRi,   [|Reg rd; Reg rm; Imm i; _; _|] ->
      stri (reg rd) (reg rm) (imm i * 4)
    | `tSTRr, [|Reg rd; Reg rm; Reg rn; _; _|] ->
      strr (reg rd) (reg rm) (reg rn)
    | `tSTRBi, [|Reg rd; Reg rm; Imm i;_;_|] ->
      strbi (reg rd) (reg rm) (imm i)
    | `tSTRBr, [|Reg rd; Reg rm; Reg rn; _; _|] ->
      strbr (reg rd) (reg rm) (reg rn)
    | `tSTRHi, [|Reg rd; Reg rm; Imm i; _; _|] ->
      strhi (reg rd) (reg rm) (imm i * 2)
    | `tSTRHr, [|Reg rd; Reg rm; Reg rn; _; _|] ->
      strhr (reg rd) (reg rm) (reg rn)
    | #opmem_multi as op,ops ->
      begin match op, Array.to_list ops with
        | `tSTMIA_UPD, Reg rd :: _ :: _ :: ar ->
          stm (reg rd) (regs ar)
        | `tLDMIA, Reg rd :: _ :: _ :: ar ->
          ldm (reg rd) (regs ar)
        | `tPUSH, _ :: _ :: ar ->
          push (regs ar)
        | `tPOP, _ :: _ :: ar ->
          let regs = regs ar in
          if has_pc regs
          then popret (remove_pc regs)
          else pop regs
        | op,_ ->
          info "unhandled multi-reg instruction: %a" pp_insn (op,ops);
          !!Insn.empty
      end
    | insn ->
      info "unhandled memory operation: %a" pp_insn insn;
      !!Insn.empty


  (* let lift_bits insn ops =
   *   let open Bits in
   *   match insn, ops with
   *   | `tSXTB, [|dest; src; _unknown; _|] -> sxtb dest src
   *   | `tSXTH, [|dest; src; _unknown; _|] -> sxth dest src
   *   | `tUXTB, [|dest; src; _unknown; _|] -> uxtb dest src
   *   | `tUXTH, [|dest; src; _unknown; _|] -> uxth dest src
   *   | _ -> [] *)

  (* these are not entirely complete *)
  let lift_branch pc opcode insn =
    let module T = Thumb_branch.Make(CT) in
    let open T in
    match opcode, (MC.Insn.ops insn : Op.t array) with
    | `tB, [|Imm dst; _; _|] -> b pc (imm dst)
    | `tBcc, [|Imm dst; Imm c; _|] -> bcc pc (cnd c) (imm dst)
    | `tBL,   [|_; _; Imm dst; _|]
    | `tBLXi, [|_; _; Imm dst; _|] -> bli pc (imm dst)
    | `tBX,   [|Reg dst; _; _|]
    | `tBLXr, [|_; _; Reg dst|] -> blr pc (reg dst)
    | insn ->
      info "unhandled branch: %a" pp_insn insn;
      !!Insn.empty

  let lift_insn addr opcode insn = match opcode with
    | #opmem as op -> lift_mem addr op insn
    | #opmov as op -> lift_move addr op insn
    | #opbranch as op -> lift_branch addr op insn
    | op ->
      info "unsupported opcode: %s" (string_of_opcode op);
      !!Insn.empty

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
      match opcode_of_sexp (Sexp.Atom (MC.Insn.name insn)) with
      | exception _ ->
        info "failed to decode MC instruction, unknown opcode: \
              %s => %a"
          (MC.Insn.asm insn)
          Sexp.pp_hum (MC.Insn.sexp_of_t insn);
        !!Insn.empty
      | opcode ->
        try
          Thumb.lift_insn addr opcode insn >>| fun sema ->
          Insn.with_basic sema insn
        with uncaught ->
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
