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

let tmp ?(name="v") typ =
  Var.create ~fresh:true ~is_virtual:true name typ


let assn d s =
  if d = Env.pc then Bil.jmp s else Bil.move d s

let bitlen = function
  | Type.Imm len -> len
  | Type.Mem (_,size) -> Size.in_bits size

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
    let z = Bil.var Env.zf in
    let c = Bil.var Env.cf in
    let v = Bil.var Env.vf in
    let n = Bil.var Env.nf in
    let f = Bil.int (Word.of_bool false) in
    let t = Bil.int (Word.of_bool true) in
    match cond with
    | `EQ -> Bil.(z = t)
    | `NE -> Bil.(z = f)
    | `CS -> Bil.(c = t)
    | `CC -> Bil.(c = f)
    | `MI -> Bil.(n = t)
    | `PL -> Bil.(n = f)
    | `VS -> Bil.(v = t)
    | `VC -> Bil.(v = f)
    | `HI -> Bil.((c = t) land (z = f))
    | `LS -> Bil.((c = f) lor  (z = t))
    | `GE -> Bil.(n = v)
    | `LT -> Bil.(n <> v)
    | `GT -> Bil.((z = f) land (n =  v))
    | `LE -> Bil.((z = t) lor  (n <> v))
    | `AL -> t in
  (* We shortcut if the condition = all *)
  match cond with
  | `AL -> stmts
  | _ -> [Bil.If (set_cond cond, stmts, [])]


let exp_of_reg reg = Bil.var (Env.of_reg reg)

let exp_of_op = function
  | Op.Reg reg -> exp_of_reg reg
  | Op.Imm word -> Bil.int word

let cast_type = function
  | Signed -> Bil.signed
  | Unsigned -> Bil.unsigned

let cast_of_sign sign size exp = Bil.cast (cast_type sign) size exp



let msb r = Bil.(cast high 1 r)
let zero ty = Bil.int (Word.zero (bitlen ty))
