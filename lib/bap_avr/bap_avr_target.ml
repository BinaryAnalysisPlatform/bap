open Core_kernel
open Bap_core_theory
module Dis = Bap.Std.Disasm_expert.Basic


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

let pcode =
  Theory.Language.declare ~package:"bap" "pcode-avr"

let provide_decoding () =
  let open KB.Syntax in
  KB.promise Theory.Label.encoding @@ fun label ->
  Theory.Label.target label >>| fun t ->
  if Theory.Target.belongs parent t
  then pcode
  else Theory.Language.unknown

let enable_ghidra () =
  Dis.register pcode @@ fun _target ->
  Dis.create ~backend:"ghidra" "avr8:LE:16:atmega256"

let enable_loader () =
  let open Bap.Std in
  let open KB.Syntax in
  let request_arch doc =
    let open Ogre.Syntax in
    match Ogre.eval (Ogre.request Image.Scheme.arch) doc with
    | Error _ -> None
    | Ok arch -> arch in
  KB.promise Theory.Unit.target @@ fun unit ->
  KB.collect Image.Spec.slot unit >>| request_arch >>| function
  | Some "avr" -> atmega328
  | _ -> Theory.Target.unknown


let load () =
  enable_ghidra ();
  enable_loader ();
  provide_decoding ()
