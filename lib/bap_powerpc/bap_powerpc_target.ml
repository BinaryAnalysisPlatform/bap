open Core_kernel
open Bap_core_theory
open Bap.Std


let package = "bap"

type r128 and r80 and r64 and r32 and r16 and r8

type 'a bitv = 'a Theory.Bitv.t Theory.Value.sort

let r128 : r128 bitv = Theory.Bitv.define 128
let r80 : r80 bitv = Theory.Bitv.define 80
let r64 : r64 bitv = Theory.Bitv.define 64
let r32 : r32 bitv = Theory.Bitv.define 32
let r16 : r16 bitv = Theory.Bitv.define 16
let r8  : r8  bitv = Theory.Bitv.define 8
let bool = Theory.Bool.t

let reg t n = Theory.Var.define t n

let array t fmt size =
  let fmt = Scanf.format_from_string fmt "%d" in
  List.init size ~f:(fun i -> reg t (sprintf fmt i))

let untyped = List.map ~f:Theory.Var.forget
let (@<) xs ys = untyped xs @ untyped ys

let name size order  =
  let order = Theory.Endianness.name order in
  sprintf "powerpc%d+%s" size (KB.Name.unqualified order)

let parent = Theory.Target.declare ~package "powerpc"

let crflags =
  List.concat @@ List.init 8 ~f:(fun group ->
      List.map ["UN"; "EQ"; "GT"; "LT"] ~f:(fun f ->
          reg bool @@ sprintf "CR%d%s" group f))

let flags = List.map ~f:(reg bool) [
    "SO"; "CA"; "OV";
    "CA32"; "OV32";
    "C"; "FL"; "FE"; "FG"; "FU"
  ] @ crflags

let define ?(parent=parent) bits endianness =
  let size = Theory.Bitv.size bits in
  let mems = Theory.Mem.define bits r8 in
  let data = Theory.Var.define mems "mem" in
  let vars = array bits "R%d" 32 @<
             array r64 "F%d" 32 @<
             array r128 "V%d" 32 @<
             flags @<
             [reg bits "CTR"; reg bits "LR"; reg bits "TAR" ] @<
             [data] in
  Theory.Target.declare ~package (name size endianness)
    ~parent
    ~bits:size
    ~endianness
    ~vars
    ~code:data
    ~data:data
    ~regs:Theory.Role.Register.[
        [general; integer], untyped@@array bits "R%d" 32;
        [general; floating], untyped@@array r64 "F%d" 32;
        [general; vector], untyped@@array r128  "V%d" 32;
        [stack_pointer], untyped@@[reg bits "R1"];
        [link], untyped@@[reg bits "LR"];
        [status], untyped flags;
        [constant; zero], untyped@@[reg bits "ZERO"];
        [zero_flag], untyped@@array bool "CR%dEQ" 8;
        [sign_flag], untyped@@array bool "CR%dLT" 8;
        [carry_flag], untyped@@[reg bool "CA"; reg bool "CA32"];
        [overflow_flag], untyped@@List.(["SO"; "OV"; "OV32"] >>| reg bool);
        [status; floating], untyped@@List.(["FL"; "FE"; "FG"; "FU"] >>| reg bool);
      ]

let powerpc32bi = define r32 Theory.Endianness.bi
let powerpc32eb = define r32 Theory.Endianness.eb ~parent:powerpc32bi
let powerpc32le = define r32 Theory.Endianness.le ~parent:powerpc32bi

let powerpc64bi = define r64 Theory.Endianness.bi
let powerpc64le = define r64 Theory.Endianness.le ~parent:powerpc64bi
let powerpc64eb = define r64 Theory.Endianness.eb ~parent:powerpc64bi

let enable_loader () =
  let open KB.Syntax in
  let request_info doc =
    let open Ogre.Syntax in
    let request =
      Ogre.request Image.Scheme.arch >>= fun arch ->
      Ogre.request Image.Scheme.is_little_endian >>= fun little ->
      Ogre.return (arch,little) in
    match Ogre.eval request doc with
    | Error _ -> None,None
    | Ok info -> info in
  KB.promise Theory.Unit.target @@ fun unit ->
  KB.collect Image.Spec.slot unit >>|
  request_info >>| function
  | Some "powerpc", None -> powerpc32bi
  | Some "powerpc64",None -> powerpc64bi
  | Some "powerpc",Some true -> powerpc32le
  | Some "powerpc64",Some true -> powerpc64le
  | Some "powerpc",Some false -> powerpc32eb
  | Some "powerpc64",Some false -> powerpc64eb
  | _ -> Theory.Target.unknown


let mapped_powerpc = Map.of_alist_exn (module Theory.Target) [
    powerpc32eb, `ppc;
    powerpc64eb, `ppc64;
    powerpc64le, `ppc64le;
  ]

let map_powerpc () =
  let open KB.Syntax in
  KB.promise Arch.unit_slot @@ fun unit ->
  KB.collect Theory.Unit.target unit >>|
  Map.find mapped_powerpc >>| function
  | Some arch -> arch
  | None -> `unknown

module Dis = Disasm_expert.Basic

let llvm_powerpc32 = Theory.Language.declare ~package "llvm-powerpc32"
let llvm_powerpc64 = Theory.Language.declare ~package "llvm-powerpc64"

let register encoding triple =
  Dis.register encoding @@ fun _ ->
  Dis.create ~backend:"llvm" triple


let enable_llvm_decoder () =
  let open KB.Syntax in
  register llvm_powerpc32 "powerpc";
  register llvm_powerpc64 "powerpc64";
  KB.promise Theory.Label.encoding @@ fun label ->
  Theory.Label.target label >>| fun t ->
  if Theory.Target.belongs parent t then
    if Theory.Target.belongs powerpc32bi t
    then llvm_powerpc32
    else llvm_powerpc64
  else Theory.Language.unknown

let pcode = Theory.Language.declare ~package "pcode-powerpc"

let enable_pcode_decoder () =
  let open KB.Syntax in
  Dis.register pcode @@begin fun t ->
    let endian =
      if Theory.(Endianness.(Target.endianness t = eb))
      then "BE" else "LE" in
    let triple = sprintf "PowerPC:%s:%d:default"
        endian (Theory.Target.bits t) in
    Dis.create ~backend:"ghidra" triple
  end;
  KB.promise Theory.Label.encoding @@begin fun label ->
    Theory.Label.target label >>| fun t ->
    if Theory.Target.belongs parent t then pcode
    else Theory.Language.unknown
  end

let load ?(backend="llvm") () =
  enable_loader ();
  map_powerpc ();
  match backend with
  | "llvm" -> enable_llvm_decoder ()
  | "ghidra" -> enable_pcode_decoder ()
  | s -> invalid_argf "Unknown disassembler backend: %S" s ()
