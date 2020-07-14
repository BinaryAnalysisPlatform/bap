open Bap_core_theory
open Base
open KB.Syntax
open KB

module Defs = Armng_defs
module Flags = Armng_flags.Flags
module Insns = Armng_insn

let package = "arm-lifter"

type insns = Defs.insn * (Defs.op list)

module ARM(Core : Theory.Core) = struct
  open Core
  open Defs
  module Env = Armng_env.Env
  module FP = Armng_env_fp.Env_fp
  module Utils = Armng_util.Utils(Core)
  module Mov = Armng_move.Mov(Core)
  module Bits = Armng_bits.Bits(Core)
  module Mul = Armng_mul.Mul(Core)
  module Special = Armng_special.Special(Core)
  module Mem = Armng_mem.Mem(Core)
  module Mem_multi = Armng_mem.Mem_Multi(Core)
  module Branch = Armng_branch.Branch(Core)
  module DSL = Armng_dsl.Make(Core)

  open Utils

  let move eff =
    KB.Object.create Theory.Program.cls >>= fun lbl ->
    blk lbl eff skip

  let ctrl eff data pc = 
    Theory.Label.for_addr pc >>= fun lbl ->
    blk lbl data eff

  let lift_move (insn : Defs.insn ) ops address =
    let ( !% ) list = DSL.expand list |> move in
    let open Mov in
    match insn, ops with
    | `MOVi,  [|dest; src; cond; _; wflag|] -> 
      !%(movi dest src cond wflag)
    | `MOVr,  [|dest; src; cond; _; wflag|] ->
      !%(movr dest src cond wflag)
    | `MOVsr, [|dest; src; sreg; simm; cond; _; wflag|] ->
      !%(movsr dest src sreg simm cond wflag)
    | `MOVsi, [|dest; src; shift_imm; cond; _; wflag|] ->
      !%(movsi dest src shift_imm cond wflag)
    | `MOVPCLR, [|cond; wflag|] ->
      let data_eff, ctrl_eff = movpclr cond wflag in
      ctrl ctrl_eff (DSL.expand data_eff) address
    | `MVNi, [|dest; src; cond; _; wflag|] ->
      !%(mvni dest src cond wflag)
    | `MVNr, [|dest; src; cond; _; wflag|] ->
      !%(mvnr dest src cond wflag)
    | `MVNsr, [|dest; src; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(mvnsr dest src shift_reg shift_imm cond wflag)
    | `MVNsi, [|dest; src; shift_imm; cond; _; wflag|] ->
      !%(mvnsi dest src shift_imm cond wflag)
    | `ANDri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(andri dest src1 src2 cond wflag)
    | `ANDrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(andrr dest src1 src2 cond wflag)
    | `ANDrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(andrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `ANDrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(andrsi dest src1 src2 shift_imm cond wflag)
    | `BICri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(bicri dest src1 src2 cond wflag)
    | `BICrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(bicrr dest src1 src2 cond wflag)
    | `BICrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(bicrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `BICrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(bicrsi dest src1 src2 shift_imm cond wflag)
    | `EORri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(eorri dest src1 src2 cond wflag)
    | `EORrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(eorrr dest src1 src2 cond wflag)
    | `EORrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(eorrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `EORrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(eorrsi dest src1 src2 shift_imm cond wflag)
    | `ORRri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(orrri dest src1 src2 cond wflag)
    | `ORRrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(orrrr dest src1 src2 cond wflag)
    | `ORRrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(orrrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `ORRrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(orrrsi dest src1 src2 shift_imm cond wflag)
    | `TEQri, [|src1; src2; cond; _|] ->
      !%(teqri src1 src2 cond)
    | `TEQrr, [|src1; src2; cond; _|] ->
      !%(teqrr src1 src2 cond)
    | `TEQrsr, [|src1; src2; shift_reg; shift_imm; cond; _|] ->
      !%(teqrsr src1 src2 shift_reg shift_imm cond)
    | `TEQrsi, [|_dest; src1; src2; shift_imm; cond; _|] ->
      !%(teqrsi src1 src2 shift_imm cond)
    | `TSTri, [|src1; src2; cond; _|] ->
      !%(tstri src1 src2 cond)
    | `TSTrr, [|src1; src2; cond; _|] ->
      !%(tstrr src1 src2 cond)
    | `TSTrsr, [|src1; src2; shift_reg; shift_imm; cond; _|] ->
      !%(tstrsr src1 src2 shift_reg shift_imm cond)
    | `TSTrsi, [|src1; src2; shift_imm; cond; _|] ->
      !%(tstrsi src1 src2 shift_imm cond)
    | `ADDri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(addri dest src1 src2 cond wflag)
    | `ADDrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(addrr dest src1 src2 cond wflag)
    | `ADDrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(addrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `ADDrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(addrsi dest src1 src2 shift_imm cond wflag)
    | `SUBri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(subri dest src1 src2 cond wflag)
    | `SUBrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(subrr dest src1 src2 cond wflag)
    | `SUBrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(subrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `SUBrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(subrsi dest src1 src2 shift_imm cond wflag)
    | `ADCri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(adcri dest src1 src2 cond wflag)
    | `ADCrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(adcrr dest src1 src2 cond wflag)
    | `ADCrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(adcrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `ADCrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(adcrsi dest src1 src2 shift_imm cond wflag)
    | `SBCri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(sbcri dest src1 src2 cond wflag)
    | `SBCrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(sbcrr dest src1 src2 cond wflag)
    | `SBCrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(sbcrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `SBCrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(sbcrsi dest src1 src2 shift_imm cond wflag)
    | `RSBri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(rsbri dest src1 src2 cond wflag)
    | `RSBrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(rsbrr dest src1 src2 cond wflag)
    | `RSBrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(rsbrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `RSBrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(rsbrsi dest src1 src2 shift_imm cond wflag)
    | `RSCri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(rscri dest src1 src2 cond wflag)
    | `RSCrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(rscrr dest src1 src2 cond wflag)
    | `RSCrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(rscrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `RSCrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(rscrsi dest src1 src2 shift_imm cond wflag)
    | `CMPri, [|src1; src2; cond; _|] ->
      !%(cmpri src1 src2 cond)
    | `CMPrr, [|src1; src2; cond; _|] ->
      !%(cmprr src1 src2 cond)
    | `CMPrsr, [|src1; src2; shift_reg; shift_imm; cond; _|] ->
      !%(cmprsr src1 src2 shift_reg shift_imm cond)
    | `CMPrsi, [|src1; src2; shift_imm; cond; _|] ->
      !%(cmprsi src1 src2 shift_imm cond)
    | `CMNri, [|src1; src2; cond; _|] ->
      !%(cmnri src1 src2 cond)
    | `CMNzrr, [|src1; src2; cond; _|] ->
      !%(cmnzrr src1 src2 cond)
    | `CMNzrsr, [|src1; src2; shift_reg; shift_imm; cond; _|] ->
      !%(cmnzrsr src1 src2 shift_reg shift_imm cond)
    | `CMNzrsi, [|src1; src2; shift_imm; cond; _|] ->
      !%(cmnzrsi src1 src2 shift_imm cond)
    (* Special Data Instructions *)
    | `MOVi16, [|`Reg dest; src; cond; _wflag|] ->
      !%(movi16 (`Reg dest) src cond _wflag)
    | `MOVTi16, [|`Reg dest; _; src; cond; _wflag|] ->
      !%(movti16 (`Reg dest) src cond _wflag)
    | _, _ -> raise (Defs.Lift_Error "unrecognized move instruction variant")

  let lift_bits insn ops =
    match insn, ops with
    | _, _ -> pass

  let lift_mult insn ops =
    match insn,ops with
    | _, _ -> pass

  let lift_mem insn ops =
    match insn, ops with
    | _, _ -> pass

  (** Branching instructions *)

  let lift_branch insn ops =
    match insn, ops with
    | _, _ -> skip

  let lift_special insn ops =
    match insn, ops with
    | _, _ -> pass

  let lift_with (addr : Bitvec.t) (insn : Armng_defs.insn)
      (ops : Armng_defs.op array) = match insn with
    | #move_insn -> lift_move insn ops addr
    | #mem_insn -> lift_mem insn ops |> move
    | #bits_insn -> lift_bits insn ops |> move
    | #mult_insn -> lift_mult insn ops |> move
    | #special_insn -> lift_special insn ops |> move
    (* this is malformed for now *)
    | #branch_insn -> ctrl (lift_branch insn ops) pass (Bitvec.zero)

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
  let module Lifter = ARM(Core) in
  match insn, mem with
  | Some insn, Some mem ->
    let addr = Word.to_bitvec@@Memory.min_addr mem in
    run_lifter label addr insn mem Lifter.lift_with
  | _ -> KB.return Insn.empty
