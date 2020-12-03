(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)

open Core_kernel
open Bap.Std

(* Operands module is used for compatibility with BAP and avoiding
   to redo the work of interfacing with the LLVM-disassembler *)

module Dis = Disasm_expert.Basic
module Insn = Dis.Insn

type ops_arr = Op.t array [@@deriving sexp_of]

let invalid_operands ?here insn =
  Result.fail @@
  Error.create ?here ("invalid operands: " ^ (Insn.ops insn |> sexp_of_ops_arr |> Sexp.to_string)) insn Dis.sexp_of_full_insn

let r ~f mem insn =
  match Insn.ops insn with
  | [| Op.Reg reg |] -> f mem reg
  | _ -> invalid_operands ~here:[%here] insn

let i ~f mem insn =
  match Insn.ops insn with
  | [| Op.Imm imm |] -> f mem imm
  | [| Op.Imm imm; Op.Reg reg |] -> f mem imm
  | _ -> invalid_operands ~here:[%here] insn

let m ~f mem insn =
  match Insn.ops insn with
  | [| Op.Reg base; Op.Imm scale; Op.Reg index; Op.Imm disp;
       Op.Reg seg|] -> f mem ~seg ~base ~scale ~index ~disp
  | _ -> invalid_operands ~here:[%here] insn

let rr ~f mem insn =
  match Insn.ops insn with
  | [| Op.Reg reg1; Op.Reg reg2 |] -> f mem reg1 reg2
  | _ -> invalid_operands ~here:[%here] insn

let ri ~f mem insn =
  match Insn.ops insn with
  | [| Op.Reg reg; Op.Imm imm |] -> f mem reg imm
  | _ -> invalid_operands ~here:[%here] insn

let ir ~f mem insn =
  match Insn.ops insn with
  | [| Op.Imm imm; Op.Reg reg |] -> f mem imm reg
  | _ -> invalid_operands ~here:[%here] insn

let rm ~f mem insn =
  match Insn.ops insn with
  | [| Op.Reg reg; Op.Reg base; Op.Imm scale; Op.Reg index;
       Op.Imm disp; Op.Reg seg|] ->
    f mem reg ~seg ~base ~scale ~index ~disp
  | _ -> invalid_operands ~here:[%here] insn

let mr ~f mem insn =
  match Insn.ops insn with
  | [| Op.Reg base; Op.Imm scale; Op.Reg index;
       Op.Imm disp; Op.Reg seg; Op.Reg reg |] ->
    f mem ~seg ~base ~scale ~index ~disp reg
  | _ -> invalid_operands ~here:[%here] insn

let mi ~f mem insn =
  match Insn.ops insn with
  | [| Op.Reg base; Op.Imm scale; Op.Reg index;
       Op.Imm disp; Op.Reg seg; Op.Imm imm |] ->
    f mem ~seg ~base ~scale ~index ~disp imm
  | _ -> invalid_operands ~here:[%here] insn

let rrr ~f mem insn =
  match Insn.ops insn with
  | [| Op.Reg reg1; Op.Reg reg2; Op.Reg reg3 |] -> f mem reg1 reg2 reg3
  | _ -> invalid_operands ~here:[%here] insn

let rri ~f mem insn =
  match Insn.ops insn with
  | [| Op.Reg reg1; Op.Reg reg2; Op.Imm imm |] -> f mem reg1 reg2 imm
  | _ -> invalid_operands ~here:[%here] insn

let rrm ~f mem insn =
  match Insn.ops insn with
  | [| Op.Reg reg1; Op.Reg reg2; Op.Reg base; Op.Imm scale;
       Op.Reg index; Op.Imm disp; Op.Reg seg|] ->
    f mem reg1 reg2 ~seg ~base ~scale ~index ~disp
  | _ -> invalid_operands ~here:[%here] insn

let rrri ~f mem insn =
  match Insn.ops insn with
  | [| Op.Reg reg1; Op.Reg reg2; Op.Reg reg3; Op.Imm imm |] ->
    f mem reg1 reg2 reg3 imm
  | _ -> invalid_operands ~here:[%here] insn
