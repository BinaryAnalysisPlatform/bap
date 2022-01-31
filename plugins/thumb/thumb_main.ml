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

let decode_opcode op =
  try Some (opcode_of_sexp (Sexp.Atom op))
  with _exn -> None

let pp_insn ppf (op,ops) =
  Format.fprintf ppf "%a(%s)"
    Sexp.pp (sexp_of_opcode op)
    (string_of_operands ops)

let (>>=?) x f = x >>= function
  | None -> KB.return Insn.empty
  | Some x -> f x

module Thumb(CT : Theory.Core) = struct

  module Thumb = Thumb_core.Make(CT)

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


  let is_pc v = String.equal (Theory.Var.name v) "PC"
  let has_pc = List.exists ~f:is_pc
  let remove_pc = List.filter ~f:(Fn.non is_pc)

  let lift_move addr opcode insn =
    let module T = Thumb_mov.Make(CT) in
    let open T in
    match opcode, (MC.Insn.ops insn : Op.t array) with
    | `tADDi3, [| Reg rd; _; Reg rn; Imm x; Imm c; _|] ->
      addi3 (reg rd) (reg rn) (imm x) (cnd c)
    | `tADDi8, [|Reg rd; _; _ ; Imm x; Imm c; _|] ->
      addi8 (reg rd) (imm x) (cnd c)
    | `tADDrr, [|Reg rd; _; Reg rn; Reg rm; Imm c; _|] ->
      addrr (reg rd) (reg rn) (reg rm) (cnd c)
    | `tADC, [|Reg rd; Reg _; Reg rn; Reg rm; Imm c; _|] ->
      adcs (reg rd) (reg rn) (reg rm) (cnd c)
    | `tADDrSPi, [|Reg rd; _; Imm x; Imm c; _;|] ->
      addrspi (reg rd) (imm x * 4) (cnd c)
    | `tADDspi, [|_; _; Imm x; Imm c; _|] ->
      addspi (imm x * 4) (cnd c)
    | `tSUBi3, [| Reg rd; _; Reg rn; Imm x; Imm c; _|] ->
      subi3 (reg rd) (reg rn) (imm x) (cnd c)
    | `tSUBi8, [|Reg rd; _; _ ; Imm x; Imm c; _|] ->
      subi8 (reg rd) (imm x) (cnd c)
    | `tSUBrr, [|Reg rd; _; Reg rn; Reg rm; Imm c; _|] ->
      subrr (reg rd) (reg rn) (reg rm) (cnd c)
    | `tSUBspi, [|_; _; Imm x; Imm c; _|] ->
      subspi (imm x * 4) (cnd c)
    | `tMOVi8, [|Reg rd; _; Imm x; Imm c; _|] ->
      movi8 (reg rd) (imm x) (cnd c)
    | `tMOVSr, [|Reg rd; Reg rn|] ->
      movsr (reg rd) (reg rn)
    | `tASRri, [|Reg rd; _; Reg rm; Imm x; Imm c; _|] ->
      asri (reg rd) (reg rm) (imm x) (cnd c)
    | `tLSRri, [|Reg rd; _; Reg rm; Imm x; Imm c; _|] ->
      lsri (reg rd) (reg rm) (imm x) (cnd c)
    | `tLSLri, [|Reg rd; _; Reg rm; Imm x; Imm c; _|] ->
      lsli (reg rd) (reg rm) (imm x) (cnd c)
    | `tCMPi8, [|Reg rn; Imm x;_;_|] ->
      cmpi8 (reg rn) (imm x)
    | `tCMPr, [|Reg rn; Reg rm;_;_|] ->
      cmpr (reg rn) (reg rm)
    | `tORR, [|Reg rd; _; _; Reg rm; Imm c; _|] ->
      lorr (reg rd) (reg rm) (cnd c)
    | #opmov,_ as insn ->
      info "unhandled move instruction: %a: %a" Bitvec.pp addr pp_insn insn;
      !!Insn.empty


  let lift_mem addr opcode insn =
    let module Mem = Thumb_mem.Make(CT) in
    let open Mem in
    let pc = W32.(addr + int 4) in
    match opcode, (MC.Insn.ops insn : Op.t array) with
    | `tLDRi,   [|Reg rd; Reg rm; Imm i; Imm c; _|]
    | `tLDRspi, [|Reg rd; Reg rm; Imm i; Imm c; _|] ->
      ldri (reg rd) (reg rm) (imm i * 4) (cnd c)
    | `tLDRr, [|Reg rd; Reg rm; Reg rn; Imm c; _|] ->
      ldrr (reg rd) (reg rm) (reg rn) (cnd c)
    | `tLDRpci, [|Reg rd; Imm i; Imm c; _|] ->
      ldrpci (reg rd) pc (imm i) (cnd c)
    | `t2LDRpci, [|Reg rd; Imm i; Imm c; _|] ->
      ldrpci (reg rd) pc (imm i) (cnd c)
    | `tLDRBi, [|Reg rd; Reg rm; Imm i; Imm c; _|] ->
      ldrbi (reg rd) (reg rm) (imm i) (cnd c)
    | `tLDRBr, [|Reg rd; Reg rm; Reg rn; Imm c; _|] ->
      ldrbr (reg rd) (reg rm) (reg rn) (cnd c)
    | `tLDRSB, [|Reg rd; Reg rm; Reg rn; Imm c; _|] ->
      ldrsb (reg rd) (reg rm) (reg rn) (cnd c)
    | `tLDRHi, [|Reg rd; Reg rm; Imm i; Imm c; _|] ->
      ldrhi (reg rd) (reg rm) (imm i * 2) (cnd c)
    | `tLDRHr, [|Reg rd; Reg rm; Reg rn; Imm c; _|] ->
      ldrhr (reg rd) (reg rm) (reg rn) (cnd c)
    | `tLDRSH, [|Reg rd; Reg rm; Reg rn; Imm c; _|] ->
      ldrsh (reg rd) (reg rm) (reg rn) (cnd c)
    | `tSTRspi, [|Reg rd; Reg rm; Imm i; Imm c; _|]
    | `tSTRi,   [|Reg rd; Reg rm; Imm i; Imm c; _|] ->
      stri (reg rd) (reg rm) (imm i * 4) (cnd c)
    | `tSTRr, [|Reg rd; Reg rm; Reg rn; Imm c; _|] ->
      strr (reg rd) (reg rm) (reg rn) (cnd c)
    | `tSTRBi, [|Reg rd; Reg rm; Imm i; Imm c;_|] ->
      strbi (reg rd) (reg rm) (imm i) (cnd c)
    | `tSTRBr, [|Reg rd; Reg rm; Reg rn; Imm c; _|] ->
      strbr (reg rd) (reg rm) (reg rn) (cnd c)
    | `tSTRHi, [|Reg rd; Reg rm; Imm i; Imm c; _|] ->
      strhi (reg rd) (reg rm) (imm i * 2) (cnd c)
    | `tSTRHr, [|Reg rd; Reg rm; Reg rn; Imm c; _|] ->
      strhr (reg rd) (reg rm) (reg rn) (cnd c)
    | #opmem_multi as op,ops ->
      begin match op, Array.to_list ops with
        | `tSTMIA_UPD, Reg rd :: Imm c :: _ :: ar
        | `tSTMIA_UPD, Reg rd :: Reg _ :: Imm c :: _ :: ar ->
          stm (reg rd) (regs ar) (cnd c)
        | `tLDMIA, Reg rd :: Imm c :: _ :: ar ->
          ldm (reg rd) (regs ar) (cnd c)
        | `tPUSH, Imm c :: _ :: ar ->
          push (regs ar) (cnd c)
        | `tPOP, Imm c :: _ :: ar ->
          let regs = regs ar in
          if has_pc regs
          then popret (remove_pc regs) (cnd c)
          else pop regs (cnd c)
        | op,_ ->
          info "unhandled multi-reg instruction: %a" pp_insn (op,ops);
          !!Insn.empty
      end
    | #opmem,_ as insn ->
      info "unhandled memory operation: %a" pp_insn insn;
      !!Insn.empty

  let lift_bits opcode insn =
    let open Thumb_bits.Make(CT) in
    match opcode, (MC.Insn.ops insn : Op.t array) with
    | `tSXTB, [|Reg rd; Reg rm; Imm c; _|] -> sx (reg rd) (reg rm) (cnd c)
    | `tSXTH, [|Reg rd; Reg rm; Imm c; _|] -> sx (reg rd) (reg rm) (cnd c)
    | `tUXTB, [|Reg rd; Reg rm; Imm c; _|] -> ux (reg rd) (reg rm) (cnd c)
    | `tUXTH, [|Reg rd; Reg rm; Imm c; _|] -> ux (reg rd) (reg rm) (cnd c)
    | #opbit,_ as insn ->
      info "unhandled bit-wise instruction: %a" pp_insn insn;
      !!Insn.empty

  let unpredictable =
    Theory.Label.for_name "arm:unpredictable" >>= CT.goto

  (* these are not entirely complete *)
  let lift_branch pc opcode insn =
    let module T = Thumb_branch.Make(CT) in
    let open T in
    match opcode, (MC.Insn.ops insn : Op.t array) with
    | `tB, [|Imm dst; _; _|] -> b pc (imm dst)
    | `tBcc, [|Imm dst; Imm c; _|] -> bcc pc (cnd c) (imm dst)
    | `tBL,   [|_; _; Imm dst; _|]
    | `tBLXi, ([|_; _; Imm dst|] | [|_; _; Imm dst; _|] )  ->
      bli pc (imm dst)
    | `tBLXr, [|_; _; Reg dst|]when is_pc (reg dst) ->
      (* blx pc is unpredictable in all versions of ARM *)
      Thumb.ctrl unpredictable
    | `tBLXr, [|_; _; Reg dst|]-> blxr pc (reg dst)
    | `tBX, [|Reg dst; _; _|]when is_pc (reg dst) -> bxi pc 0
    | `tBX, [|Reg dst;_;_|] -> bxr (reg dst)
    | `tCBNZ, [|Reg rn; Imm c|] -> cbnz pc (reg rn) (imm c)
    | `tCBZ, [|Reg rn; Imm c|] -> cbz pc (reg rn) (imm c)
    | #opbranch,_ as insn ->
      info "unhandled branch: %a" pp_insn insn;
      !!Insn.empty

  let lift_insn addr opcode insn = match opcode with
    | #opmem as op -> lift_mem addr op insn
    | #opmov as op -> lift_move addr op insn
    | #opbranch as op -> lift_branch addr op insn
    | #opbit as op -> lift_bits op insn
end


module Main = struct
  open Bap.Std

  let load () =
    KB.promise Theory.Semantics.slot @@ fun label ->
    KB.collect Theory.Label.encoding label >>= fun encoding ->
    if Theory.Language.equal Target.llvm_t32 encoding then
      KB.collect MC.Insn.slot label >>=? fun insn ->
      KB.collect Memory.slot label >>=? fun mem ->
      Theory.instance () >>= Theory.require >>= fun (module Core) ->
      let module Thumb = Thumb(Core) in
      let addr = Word.to_bitvec@@Memory.min_addr mem in
      match decode_opcode (MC.Insn.name insn) with
      | None -> !!Insn.empty
      | Some opcode ->
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
