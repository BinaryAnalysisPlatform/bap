open Bap_core_theory
open Core_kernel
open Bap.Std

let package = "bap"

type r256 and r128 and r80 and r64 and r32 and r16 and r8

type 'a bitv = 'a Theory.Bitv.t Theory.Value.sort

let r256 : r128 bitv = Theory.Bitv.define 256
let r128 : r128 bitv = Theory.Bitv.define 128
let r80 : r80 bitv = Theory.Bitv.define 80
let r64 : r64 bitv = Theory.Bitv.define 64
let r32 : r32 bitv = Theory.Bitv.define 32
let r16 : r16 bitv = Theory.Bitv.define 16
let r8  : r8  bitv = Theory.Bitv.define 8
let bool = Theory.Bool.t


let reg t n = Theory.Var.define t n

let untyped = List.map ~f:Theory.Var.forget

let (@<) xs ys = untyped xs @ untyped ys

let array ?(index=string_of_int) t pref size =
  List.init size ~f:(fun i -> reg t (pref ^ index i))

module Role = struct
  let index = Theory.Role.declare ~package:"x86" "index"
  let segment = Theory.Role.declare ~package:"x86" "segment"
end

module M16 = struct
  let main = [
    reg r16 "AX";
    reg r16 "BX";
    reg r16 "CX";
    reg r16 "DX";
  ]

  let index = [
    reg r16 "SI";
    reg r16 "DI";
    reg r16 "BP";
    reg r16 "SP";
  ]

  let segment = [
    reg r16 "CS";
    reg r16 "DS";
    reg r16 "ES";
    reg r16 "SS";
  ]

  let flags = [
    reg bool "CF";
    reg bool "PF";
    reg bool "AF";
    reg bool "ZF";
    reg bool "SF";
    reg bool "TF";
    reg bool "IF";
    reg bool "DF";
    reg bool "OF";
  ]

  let mems = Theory.Mem.define r16 r8
  let data = Theory.Var.define mems "mem"
  let vars = main @< index @< segment @< flags @< [data]
  let status_regs = Theory.Role.Register.[
      [status], untyped flags;
      [integer], untyped [
        reg bool "CF";
        reg bool "PF";
        reg bool "AF";
        reg bool "ZF";
        reg bool "SF";
        reg bool "OF";
      ];
    ]

  let regs = Theory.Role.Register.[
      [general; integer], main @< index @< segment;
      [stack_pointer], untyped [reg r16 "SP"];
      [frame_pointer], untyped [reg r16 "BP"];
      [Role.index], untyped index;
      [Role.segment], untyped segment;
    ] @ status_regs
end

module M32 = struct
  let main = [
    reg r32 "EAX";
    reg r32 "EBX";
    reg r32 "ECX";
    reg r32 "EDX";
  ]

  let index = [
    reg r32 "ESI";
    reg r32 "EDI";
    reg r32 "EBP";
    reg r32 "ESP";
  ]

  let segment = [
    reg r16 "CS";
    reg r16 "DS";
    reg r16 "ES";
    reg r16 "SS";
    reg r16 "FS";
    reg r16 "GS";
  ]

  let flags = M16.flags

  let stx = array r80 "ST" 8
  let mmx = array r64 "MM" 8
  let xmmx = array r128 "XMM" 8

  let mems = Theory.Mem.define r32 r8
  let data = Theory.Var.define mems "mem"

  let i386 = main @< index @< segment @< flags @< [data]
  let i486 = i386 @< stx
  let i586 = i486 @< mmx
  let i686 = i586 @< xmmx

  let i386regs = Theory.Role.Register.[
      [general; integer], main @< index @< segment;
      [stack_pointer], untyped [reg r32 "ESP"];
      [frame_pointer], untyped [reg r32 "EBP"];
      [Role.index], untyped index;
      [Role.segment], untyped segment;
    ] @ M16.status_regs

  let i486regs = i386regs @ Theory.Role.Register.[
      [general; floating], untyped stx;
    ]

  let i586regs = i486regs @ Theory.Role.Register.[
      [general; floating], untyped mmx;
    ]

  let i686regs = i586regs @ Theory.Role.Register.[
      [general; floating], untyped xmmx;
    ]
end

module M64 = struct
  let main = [
    reg r64 "RAX";
    reg r64 "RBX";
    reg r64 "RCX";
    reg r64 "RDX";
  ]

  let index = [
    reg r64 "RSI";
    reg r64 "RDI";
    reg r64 "RBP";
    reg r64 "RSP";
  ]

  let segment = [
    reg r16 "CS";
    reg r16 "DS";
    reg r16 "ES";
    reg r16 "SS";
  ]

  let rx = array r64 "R" 8
      ~index:(fun i -> string_of_int (i+8))

  let stx = M32.stx
  let mmx = M32.mmx
  let ymmx = array r256 "YMM" 16

  let flags = M32.flags
  let mems = Theory.Mem.define r64 r8
  let data = Theory.Var.define mems "mem"

  let vars = main @< index @< segment @< rx @< stx @< mmx @< ymmx @<
             flags @< [data]

  let regs =  Theory.Role.Register.[
      [general; integer], main @< index @< segment @< rx;
      [general; floating], stx @< mmx @< ymmx;
      [stack_pointer], untyped [reg r64 "RSP"];
      [frame_pointer], untyped [reg r64 "RBP"];
      [Role.index], untyped index;
      [Role.segment], untyped segment;
    ] @ M16.status_regs
end

let parent = Theory.Target.declare ~package "x86"

let i86 = Theory.Target.declare ~package "i86"
    ~parent
    ~nicknames:["8086"]
    ~bits:16
    ~byte:8
    ~data:M16.data
    ~code:M16.data
    ~vars:M16.vars
    ~regs:M16.regs
    ~endianness:Theory.Endianness.le

let i186 = Theory.Target.declare ~package "i186"
    ~parent:i86
    ~nicknames:["80186"; "186"]

let i286 = Theory.Target.declare ~package "i286"
    ~parent:i186
    ~nicknames:["80286"; "286"]

let i386 = Theory.Target.declare ~package "i386"
    ~parent:i286
    ~nicknames:["386"; "80386"]
    ~bits:32
    ~data:M32.data
    ~code:M32.data
    ~vars:M32.i386
    ~regs:M32.i386regs

let i486 = Theory.Target.declare ~package "i486"
    ~parent:i386
    ~nicknames:["486"; "80486"]
    ~vars:M32.i486
    ~regs:M32.i486regs

let i586 = Theory.Target.declare ~package "i586"
    ~parent:i486
    ~nicknames:["586"; "80586"; "p5"]
    ~vars:M32.i586
    ~regs:M32.i586regs

let i686 = Theory.Target.declare ~package "i686"
    ~parent:i586
    ~nicknames:["686"; "80686"; "p6"]
    ~vars:M32.i686
    ~regs:M32.i686regs

let amd64 = Theory.Target.declare ~package "amd64"
    ~parent:i686
    ~nicknames:["x64"; "x86_64"; "x86-64"; ]
    ~bits:64
    ~data:M64.data
    ~code:M64.data
    ~vars:M64.vars
    ~regs:M64.regs


let family = [amd64; i686; i586; i486; i386; i86]

let enable_loader () =
  let open KB.Syntax in
  KB.Rule.(declare ~package "x86-target" |>
           require Image.Spec.slot |>
           provide Theory.Unit.target |>
           comment "computes target from the OGRE specification");
  let request_arch doc =
    match Ogre.eval (Ogre.request Image.Scheme.arch) doc with
    | Error _ -> None
    | Ok arch -> arch in
  KB.promise Theory.Unit.target @@ fun unit ->
  KB.collect Image.Spec.slot unit >>|
  request_arch >>| function
  | Some ("amd64"|"x86-64"|"x86_64") -> amd64
  | Some ("x86"|"i386"|"i486"|"i586"|"i686") -> i686
  | _ -> Theory.Target.unknown

let enable_arch () =
  let open KB.Syntax in
  KB.Rule.(declare ~package "x86-arch" |>
           require Theory.Unit.target |>
           provide Arch.unit_slot |>
           comment "computes Arch.t from the unit's target");
  KB.promise Arch.unit_slot @@ fun unit ->
  KB.collect Theory.Unit.target unit >>| fun t ->
  if Theory.Target.belongs amd64 t
  then `x86_64
  else if Theory.Target.belongs i386 t
  then `x86
  else `unknown

let llvm_x86_encoding =
  Theory.Language.declare ~package "llvm-x86"

let llvm_x86_64_encoding =
  Theory.Language.declare ~package "llvm-x86_64"

let pcode =
  Theory.Language.declare ~package "pcode-x86"

let register_x86_llvm_disassembler () =
  Disasm_expert.Basic.register llvm_x86_encoding @@ fun _ ->
  Disasm_expert.Basic.create ~backend:"llvm" "x86"

let register_x86_64_llvm_disassembler () =
  Disasm_expert.Basic.register llvm_x86_64_encoding @@ fun _ ->
  Disasm_expert.Basic.create ~backend:"llvm" "x86_64"

let register_sleigh_disassembler () =
  Disasm_expert.Basic.register pcode @@ fun target ->
  let target = if Theory.Target.belongs amd64 target
    then "x86:LE:64:default"
    else "x86:LE:32:default" in
  Disasm_expert.Basic.create ~backend:"ghidra" target

let enable_decoder backend =
  let open KB.Syntax in
  register_x86_llvm_disassembler ();
  register_x86_64_llvm_disassembler ();
  register_sleigh_disassembler ();
  KB.promise Theory.Label.encoding @@ fun label ->
  Theory.Label.target label >>| fun t ->
  if Theory.Target.belongs parent t
  then
    if String.equal backend "llvm" then
      if Theory.Target.belongs amd64 t
      then llvm_x86_64_encoding else
      if Theory.Target.belongs parent t
      then llvm_x86_encoding
      else Theory.Language.unknown
    else pcode
  else Theory.Language.unknown

let load ?(backend="llvm") () =
  enable_loader ();
  enable_arch ();
  enable_decoder backend
