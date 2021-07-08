open Core_kernel
open Bap_core_theory
module Dis = Bap.Std.Disasm_expert.Basic


let package = "bap"

type r16 and r8

type 'a bitv = 'a Theory.Bitv.t Theory.Value.sort
type reg = r8 Theory.Bitv.t Theory.var


let r16 : r16 bitv = Theory.Bitv.define 16
let r8  : r8  bitv = Theory.Bitv.define 8
let bool = Theory.Bool.t

let reg t n = Theory.Var.define t n

let array ?(rev=false) ?(start=0) ?(index=string_of_int) t pref size =
  let stop = if rev then start-size else start+size in
  let stride = if rev then -1 else 1 in
  List.range ~stride start stop |>
  List.map ~f:(fun i -> reg t (pref ^ index i))

let untyped = List.map ~f:Theory.Var.forget
let (@<) xs ys = untyped xs @ untyped ys

let regs t = List.map ~f:(reg t)
let nums = array r8 "R" 24
let wxyz = regs r8 [
    "Wlo"; "Whi";
    "Xlo"; "Xhi";
    "Ylo"; "Yhi";
    "Zlo"; "Zhi";
  ]
let gpr = nums @< wxyz
let sp = reg r16 "SP"
let flags = regs bool [
    "CF"; "ZF"; "NF"; "VF"; "SF"; "HF"; "TF"; "IF"
  ]


let datas = Theory.Mem.define r16 r8
let codes = Theory.Mem.define r16 r16

let data = reg datas "data"
let code = reg codes "code"

let parent = Theory.Target.declare ~package "avr8"
    ~bits:8
    ~byte:8
    ~endianness:Theory.Endianness.le


let tiny = Theory.Target.declare ~package "avr8-tiny"
    ~parent
    ~data
    ~code
    ~vars:(gpr @< [sp] @< flags @< [data] @< [code])

let mega = Theory.Target.declare ~package "avr8-mega"
    ~parent:tiny

let xmega = Theory.Target.declare ~package "avr8-xmega"
    ~parent:mega

module Gcc = struct
  let abi = Theory.Abi.declare ~package "avr-gcc"
  let wreg = regs r8 ["Whi"; "Wlo"]
  let args = wreg @ array ~rev:true ~start:23 r8 "R" 16
  let rets = wreg @ array ~rev:true ~start:23 r8 "R" 6
  let regs = Theory.Role.Register.[
      [general; integer], gpr;
      [function_argument], untyped args;
      [function_return], untyped rets;
      [stack_pointer], untyped [sp];
      [caller_saved], rets @< regs r8 ["R0"; "Xlo"; "Xhi"; "Zlo"; "Zhi"];
      [callee_saved], array ~start:1 r8 "R" 17 @< regs r8 ["Ylo"; "Yhi"];
    ]

  let target parent name =
    Theory.Target.declare ~package name ~regs ~parent ~abi

  let tiny = target tiny "avr8-tiny-gcc"
  let mega = target mega "avr8-mega-gcc"
  let xmega = target xmega "avr8-xmega-gcc"
end



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
  | Some "avr" -> Gcc.mega
  | _ -> Theory.Target.unknown


let load () =
  enable_ghidra ();
  enable_loader ();
  provide_decoding ()
