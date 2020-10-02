open Core_kernel
open Bap_core_theory

let package = "bap"

type r64 and r32 and r16 and r8

type 'a bitv = 'a Theory.Bitv.t Theory.Value.sort

let r64 : r64 bitv = Theory.Bitv.define 64
let r32 : r32 bitv = Theory.Bitv.define 32
let r16 : r16 bitv = Theory.Bitv.define 16
let r8  : r8  bitv = Theory.Bitv.define 8
let bool = Theory.Bool.t

let reg t n = Theory.Var.define t n

let array ?(index=string_of_int) t pref size =
  List.init size ~f:(fun i -> reg t (pref ^ index i))

let untyped = List.map ~f:Theory.Var.forget
let (@<) xs ys = untyped xs @ untyped ys

let mems = Theory.Mem.define r64 r8

let gpr = array r64 "R" 16
let fpr = array r64 "F" 16
let mem = reg mems "mem"

let vars = gpr @< fpr @< [mem]

let parent = Theory.Target.declare ~package "systemz"

let z9 = Theory.Target.declare ~package "systemz9" ~parent
    ~bits:64
    ~code:mem
    ~data:mem
    ~vars

let llvm_encoding = Theory.Language.declare ~package "llvm-systemz"
