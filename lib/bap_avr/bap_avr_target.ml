open Core_kernel
open Bap_core_theory

let package = "bap"

type r16 and r8

type 'a bitv = 'a Theory.Bitv.t Theory.Value.sort

let r16 : r16 bitv = Theory.Bitv.define 16
let r8  : r8  bitv = Theory.Bitv.define 8
let bool = Theory.Bool.t

let reg t n = Theory.Var.define t n

let array ?(index=string_of_int) t pref size =
  List.init size ~f:(fun i -> reg t (pref ^ index i))

let untyped = List.map ~f:Theory.Var.forget
let (@<) xs ys = untyped xs @ untyped ys

let gpr = array r8 "R" 32
let sp = reg r16 "SP"
let flags = List.map ~f:(reg bool) [
    "CF"; "ZF"; "NF"; "VF"; "SF"; "HF"; "TF"; "IF"
  ]

let datas = Theory.Mem.define r16 r8
let codes = Theory.Mem.define r16 r16

let data = reg datas "data"
let code = reg codes "code"

let parent = Theory.Target.declare ~package "avr"
    ~bits:8
    ~byte:8
    ~endianness:Theory.Endianness.le


let atmega328 = Theory.Target.declare ~package "ATmega328"
    ~parent
    ~data
    ~code
    ~vars:(gpr @< [sp] @< flags @< [data] @< [code])


let llvm_avr16 = Theory.Language.declare ~package "llvm-avr16"
