open Core_kernel.Std
open Bap.Std

open Riscv_types
module Env = Riscv_env

let fail here fmt =
  ksprintf (fun msg ->
      let msg = sprintf "%s: %s"
          (Source_code_position.to_string here) msg in
      raise (Lifting_failed msg)) fmt

let assert_reg loc = function
  | `Imm _ -> fail loc "expected reg"
  | `Reg reg -> reg

let assert_imm loc = function
  | `Reg _ -> fail loc "expected imm"
  | `Imm imm -> imm


let assert_cond loc op =
  match Riscv_cond.create (assert_imm loc op) with
  | Ok cond -> cond
  | Error err -> fail loc "bad argument (cond): %s" @@
    Error.to_string_hum err

let tmp ?(name="v") typ =
  Var.create ~fresh:true ~is_virtual:true name typ

let bitlen = function
  | Type.Imm len -> len
  | Type.Mem (_,size) -> Size.in_bits size

let exec
    (stmts : stmt list)
    ?(flags : stmt list option)
    (cond : op) : stmt list =
  let cond = assert_cond [%here] cond in
  let stmts = match flags with
    | Some f -> stmts @ f
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
    | `GE -> Bil.(n = v)
    | `GEU -> Bil.(n = v)
    | `LT -> Bil.(n <> v)
    | `LTU -> Bil.(n <> v)
    | `AL -> t in
  (* We shortcut if the condition = all *)
  match cond with
  | `AL -> stmts
  | _ -> [Bil.If (set_cond cond, stmts, [])]


let exp_of_reg reg = Bil.var (Env.of_reg reg)

let exp_of_op = function
  | `Reg reg -> exp_of_reg reg
  | `Imm word -> Bil.int word

let cast_type = function
  | Signed -> Bil.signed
  | Unsigned -> Bil.unsigned

let cast_of_sign sign size exp = Bil.cast (cast_type sign) size exp



let msb r = Bil.(cast high 1 r)
let zero ty = Bil.int (Word.zero (bitlen ty))
