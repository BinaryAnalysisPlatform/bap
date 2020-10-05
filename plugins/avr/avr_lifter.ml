open Core_kernel
open Bap_core_theory
open Bap.Std

open KB.Syntax
include Bap_main.Loggers()

module Target = Bap_avr_target
module MC = Disasm_expert.Basic

type r1
type 'a bitv = 'a Theory.Bitv.t Theory.Value.sort

let r1 : r1 bitv = Theory.Bitv.define 1

let make_regs regs =
  let regs =
    List.mapi regs ~f:(fun i r -> (i,r)) |>
    Map.of_alist_exn (module Int) in
  Map.find_exn regs

let gpr = make_regs Target.gpr
let regnum s = Scanf.sscanf s "R%d" ident

let require_gpr insn pos f =
  match (MC.Insn.ops insn).(pos) with
  | Op.Reg r -> f (gpr (regnum (Reg.name r)))
  | _ -> KB.return Insn.empty

let require_imm insn pos f =
  match (MC.Insn.ops insn).(pos) with
  | Op.Imm x -> f (Option.value_exn (Imm.to_int x))
  | _ -> KB.return Insn.empty

let hf = Theory.Var.define r1 "HF"
let cf = Theory.Var.define r1 "CF"
let nf = Theory.Var.define r1 "NF"
let vf = Theory.Var.define r1 "VF"
let sf = Theory.Var.define r1 "SF"
let zf = Theory.Var.define r1 "ZF"

module M8 = Bitvec.M8

module Avr(CT : Theory.Core) = struct
  open Target
  let rec seq = function
    | [] -> CT.perform Theory.Effect.Sort.bot
    | [x] -> x
    | x :: xs -> CT.seq x @@ seq xs

  let const x = CT.int r8 (M8.int x)

  let bit0 = CT.int r1 (Bitvec.M1.bool false)
  let bit1 = CT.int r1 (Bitvec.M1.bool true)
  let flag x = CT.ite x bit1 bit0

  let nth x n =
    CT.(extract r1 (const n) (const n) x)

  let data xs =
    KB.Object.create Theory.Program.cls >>= fun lbl ->
    CT.blk lbl (seq xs) (seq [])

  let (&&) = CT.logand
  let (||) = CT.logor
  let not = CT.not

  let bit reg n body =
    nth CT.(var reg) n >>= fun expr ->
    Theory.Var.scoped r1 @@ fun v ->
    CT.let_ v !!expr (body (CT.var v))

  let halfcarry ~r ~rd ~rr =
    bit r  3 @@ fun r3 ->
    bit rd 3 @@ fun rd3 ->
    bit rr 3 @@ fun rr3 ->
    rd3 && rr3 || rr3 && not r3 || not r3 && rd3

  let fullcarry ~r ~rd ~rr =
    bit r  7 @@ fun r7 ->
    bit rd 7 @@ fun rd7 ->
    bit rr 7 @@ fun rr7 ->
    rd7 && rr7 || rr7 && not r7 || r7 && not rd7

  let overflow ~r ~rd ~rr =
    bit r  7 @@ fun r7 ->
    bit rd 7 @@ fun rd7 ->
    bit rr 7 @@ fun rr7 ->
    rd7 && rr7 && not r7 || not rd7 && not rr7 && r7


  let with_result rd f =
    Theory.Var.fresh (Theory.Var.sort rd) >>= fun v ->
    f v >>= fun effs ->
    data (effs @ CT.[set rd (var v)])

  let (:=) = CT.set
  let (+) = CT.add


  let adc insn =
    require_gpr insn 1 @@ fun rd ->
    require_gpr insn 2 @@ fun rr ->
    with_result rd @@ fun r ->
    KB.return @@
    CT.[
      r := var rd + var rr + unsigned r8 (var cf);
      hf := halfcarry ~r ~rd ~rr;
      nf := nth (var r) 7;
      vf := overflow ~r ~rd ~rr;
      sf := var nf || var vf;
      zf := flag (is_zero (var r));
      cf := fullcarry ~r ~rd ~rr;
    ]


  let add insn =
    require_gpr insn 1 @@ fun rd ->
    require_gpr insn 2 @@ fun rr ->
    with_result rd @@ fun r ->
    KB.return @@
    CT.[
      r := var rd + var rr;
      hf := halfcarry ~r ~rd ~rr;
      nf := nth (var r) 7;
      vf := overflow ~r ~rd ~rr;
      sf := var nf || var vf;
      zf := flag (is_zero (var r));
      cf := fullcarry ~r ~rd ~rr;
    ]

  let ldi insn =
    require_gpr insn 0 @@ fun rd ->
    require_imm insn 1 @@ fun k ->
    data [
      rd := const k
    ]
end

let lifter label : unit Theory.eff =
  KB.collect MC.Insn.slot label >>= function
  | None -> KB.return Insn.empty
  | Some insn ->
    Theory.instance () >>= Theory.require >>= fun (module Core) ->
    let module Avr = Avr(Core) in
    let open Avr in
    insn |> match MC.Insn.name insn with
    | "ADCRdRr" -> adc
    | "ADDRdRr" -> add
    | "LDIRdK"  -> ldi
    | code ->
      info "unsupported opcode: %s" code;
      fun _ -> KB.return Insn.empty

let load () =
  KB.promise Theory.Semantics.slot lifter
