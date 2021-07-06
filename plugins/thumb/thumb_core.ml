open Core_kernel
open Bap_core_theory

open KB.Syntax

type r1 and r8 and r16 and r32

type 'a reg = 'a Theory.Bitv.t Theory.var
type 'a sort = 'a Theory.Bitv.t Theory.Value.sort

let s1  : r1 sort = Theory.Bitv.define 1
let s8  : r8 sort = Theory.Bitv.define 8
let s16 : r16 sort = Theory.Bitv.define 16
let s32 : r32 sort = Theory.Bitv.define 32

let mems = Theory.Mem.define s32 s8
let mem = Theory.Var.define mems "mem"

let r0 = Theory.Var.define s32 "R0"
let r1 = Theory.Var.define s32 "R1"
let r2 = Theory.Var.define s32 "R2"
let r3 = Theory.Var.define s32 "R3"
let r4 = Theory.Var.define s32 "R4"
let r5 = Theory.Var.define s32 "R5"
let r6 = Theory.Var.define s32 "R6"
let r7 = Theory.Var.define s32 "R7"
let r8 =  Theory.Var.define s32 "R8"
let r9 =  Theory.Var.define s32 "R9"
let r10 = Theory.Var.define s32 "R10"
let r11 = Theory.Var.define s32 "R11"
let r12 = Theory.Var.define s32 "R12"
let lr =  Theory.Var.define s32 "LR"
let sp =  Theory.Var.define s32 "SP"

let nf = Theory.Var.define s1 "NF"
let zf = Theory.Var.define s1 "ZF"
let cf = Theory.Var.define s1 "CF"
let vf = Theory.Var.define s1 "VF"
let qf = Theory.Var.define s1 "QF"
let tf = Theory.Var.define s1 "TF"

module W1 = Bitvec.M1
module W8 = Bitvec.M8
module W32 = Bitvec.M32

module Make(CT : Theory.Core) = struct
  let rec seq = function
    | [] -> CT.perform Theory.Effect.Sort.bot
    | [x] -> x
    | x :: xs -> CT.seq x @@ seq xs

  let foreach xs f = seq @@ List.concat_map xs ~f
  let foreachi xs f = seq @@ List.concat_mapi xs ~f
  let bitv x = CT.int s32 x
  let const x = bitv (W32.int x)

  let var = CT.var

  let load s p = CT.loadw s CT.b0 (var mem) p
  let store p x = CT.set mem (CT.storew CT.b0 (var mem) p x)
  let bit0 = CT.int s1 (W1.bool false)
  let bit1 = CT.int s1 (W1.bool true)
  let bit v = CT.ite v bit1 bit0
  let bool flag = CT.(eq bit1 flag)

  let is_set flag = bool flag
  let is_clear flag = CT.(eq bit0 flag)

  let null = Theory.Label.null
  let label = Theory.Label.fresh
  let data eff = CT.blk null (seq eff) (seq [])
  let ctrl eff = CT.blk null (seq []) eff

  let goto dst =
    Theory.Label.for_addr dst >>= fun dst ->
    ctrl (CT.goto dst)

  let with_result rd f =
    Theory.Var.fresh (Theory.Var.sort rd) >>= fun v ->
    data (f v @ CT.[set rd (var v)])

  let nth n x = CT.extract s1 (const n) (const n) x

  let msb x = nth 31 x
  let lsb x = nth 0 x

  let is_zero x = bit (CT.is_zero x)

  module Syntax = struct

    let (:=) = CT.set
    let (+) = CT.add
    let (-) = CT.sub
    let (+=) r x = r := var r + x
    let (-=) r x = r := var r - x
    let (<--) = store
    let (+>) pc off = W32.(pc + int Int.(off + 4))
    let (=) = CT.eq
    let (<>) = CT.neq
    let (<) = CT.ult
    let (<=) = CT.ule
    let (>) = CT.ugt
    let (>=) = CT.uge
    let (<$) = CT.slt
    let (<=$) = CT.sle
    let (>$) = CT.sgt
    let (>=$) = CT.sge
    let (&&) = CT.and_
    let (||) = CT.or_
    let (land) = CT.logand
    let (lor) = CT.logor
    let (lxor) = CT.logxor
    let (lsl) = CT.lshift
    let (lsr) = CT.rshift
    let (asr) = CT.arshift
    let not = CT.inv
    let lnot = CT.not

    let (~@) = CT.var
  end
end
