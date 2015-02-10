open Core_kernel.Std
open Bap_types.Std

open Bap_disasm_arm_types
module Arm = Bap_disasm_arm
module Env = Bap_disasm_arm_env

let fail here fmt =
  ksprintf (fun msg ->
      let msg = sprintf "%s: %s"
          (Source_code_position.to_string here) msg in
      raise (Lifting_failed msg)) fmt

let assert_reg loc = function
  | Op.Imm _ -> fail loc "expected reg"
  | Op.Reg reg -> reg

let assert_imm loc = function
  | Op.Reg _ -> fail loc "expected imm"
  | Op.Imm imm -> imm


let assert_cond loc op =
  match Arm.Cond.create (assert_imm loc op) with
  | Ok cond -> cond
  | Error err -> fail loc "bad argument (cond): %s" @@
    Error.to_string_hum err


let assn d s =
  if d = Env.pc then Stmt.jmp s else Stmt.move d s

let bitlen = function
  | Type.Imm len -> len
  | Type.Mem (_,size) -> Size.to_bits size

let exec
    (stmts : stmt list)
    ?(flags : stmt list option)
    ?(wflag : op option)
    (cond : op) : stmt list =
  (* write to the flags if wflag is CPSR *)
  let cond = assert_cond _here_ cond in
  let stmts = match flags, wflag with
    | Some f, Some (Op.Reg `CPSR) -> stmts @ f
    | _ -> stmts in
  (* generates an expression for the given McCond *)
  let set_cond mccond =
    let z = Exp.var Env.zf in
    let c = Exp.var Env.cf in
    let v = Exp.var Env.vf in
    let n = Exp.var Env.nf in
    let f = Exp.int (Word.of_bool false) in
    let t = Exp.int (Word.of_bool true) in
    match cond with
    | `EQ -> Exp.(z = t)
    | `NE -> Exp.(z = f)
    | `CS -> Exp.(c = t)
    | `CC -> Exp.(c = f)
    | `MI -> Exp.(n = t)
    | `PL -> Exp.(n = f)
    | `VS -> Exp.(v = t)
    | `VC -> Exp.(v = f)
    | `HI -> Exp.((c = t) land (z = f))
    | `LS -> Exp.((c = f) lor  (z = t))
    | `GE -> Exp.(n = v)
    | `LT -> Exp.(n <> v)
    | `GT -> Exp.((z = f) land (n =  v))
    | `LE -> Exp.((z = t) lor  (n <> v))
    | `AL -> t in
  (* We shortcut if the condition = all *)
  match cond with
  | `AL -> stmts
  | _ -> [Stmt.If (set_cond cond, stmts, [])]


let exp_of_reg reg = Exp.var (Env.of_reg reg)

let exp_of_op = function
  | Op.Reg reg -> exp_of_reg reg
  | Op.Imm word -> Exp.int word

let cast_type = function
  | Signed -> Exp.signed
  | Unsigned -> Exp.unsigned

let cast_of_sign sign size exp = Exp.cast (cast_type sign) size exp



let msb r = Exp.(cast high 1 r)
let zero ty = Exp.int (Word.zero (bitlen ty))
