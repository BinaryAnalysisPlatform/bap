open Bap_core_theory
open Core_kernel[@@warning "-D"]
open Bap_demangle.Std
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

let lower xs _ ys =
  List.map2_exn xs ys ~f:Theory.Alias.(fun x y -> def x [unk; reg y])

let are f x y = f x y

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

  let aliases = M16.main @< M16.index

  let i386regs = Theory.Role.Register.[
      [general; integer], main @< index @< segment;
      [stack_pointer], untyped [reg r32 "ESP"];
      [frame_pointer], untyped [reg r32 "EBP"];
      [Role.index], untyped index;
      [Role.segment], untyped segment;
      [alias], aliases;
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

  let aliasing =
    lower main are M16.main
    @ lower index are M16.index
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
  let xmmx = array r128 "XMM" 16
  let ymmx = array r256 "YMM" 16
  let mxsr = reg r32 "MXCSR"

  let flags = M32.flags
  let mems = Theory.Mem.define r64 r8
  let data = Theory.Var.define mems "mem"

  let aliases = M32.aliases @< M32.main @< M32.index @< xmmx

  let vars = main @< index @< segment @< rx @< stx @< mmx @< ymmx @<
             flags @< [data] @< [mxsr]

  let regs =  Theory.Role.Register.[
      [general; integer], main @< index @< segment @< rx;
      [general; floating], stx @< mmx @< ymmx;
      [stack_pointer], untyped [reg r64 "RSP"];
      [frame_pointer], untyped [reg r64 "RBP"];
      [Role.index], untyped index;
      [Role.segment], untyped segment;
      [status], untyped [mxsr];
      [alias], aliases;
    ] @ M16.status_regs

  let aliasing =
    M32.aliasing
    @ lower main are M32.main
    @ lower index are M32.index
    @ lower ymmx are xmmx
end

let parent = Theory.Target.declare ~package "x86"
    ~bits:16
    ~byte:8

let i86 = Theory.Target.declare ~package "i86"
    ~parent
    ~nicknames:["8086"]
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
    ~aliasing:M32.aliasing



let i486 = Theory.Target.declare ~package "i486"
    ~parent:i386
    ~nicknames:["486"; "80486"]
    ~vars:M32.i486
    ~regs:M32.i486regs
    ~aliasing:M32.aliasing

let i586 = Theory.Target.declare ~package "i586"
    ~parent:i486
    ~nicknames:["586"; "80586"; "p5"]
    ~vars:M32.i586
    ~regs:M32.i586regs
    ~aliasing:M32.aliasing

let i686 = Theory.Target.declare ~package "i686"
    ~parent:i586
    ~nicknames:["686"; "80686"; "p6"]
    ~vars:M32.i686
    ~regs:M32.i686regs
    ~aliasing:M32.aliasing

let amd64 = Theory.Target.declare ~package "x86_64"
    ~parent:i686
    ~nicknames:["amd64"; "x64"; "x86-64"; ]
    ~bits:64
    ~data:M64.data
    ~code:M64.data
    ~vars:M64.vars
    ~regs:M64.regs
    ~aliasing:M64.aliasing


module Abi = struct
  open Bap_c.Std

  module Arg = C.Abi.Arg
  open Arg.Language

  module Abi = struct
    let abi = Theory.Abi.declare ~package
    let cdecl = Theory.Abi.cdecl
    let pascal = abi "pascal"
    let fortran = abi "fortran"
    let fastcall = Theory.Abi.fastcall
    let stdcall = Theory.Abi.stdcall
    let thiscall = abi "thiscall"
    let vectorcall = abi "vectorcall"
    let watcom = Theory.Abi.watcom
    let ms = Theory.Abi.ms
    let sysv = Theory.Abi.gnu
  end

  let is_integer =
    any C.Type.[is_integer; is_pointer; is_function]

  let is_composite =
    any C.Type.[is_structure; is_union]

  let is_sse : C.Type.t -> bool = function
    | `Basic {t=(`float|`double)} -> true
    | _ -> false

  let is_csse : C.Type.t -> bool = function
    | `Basic {t=(`cfloat|`cdouble)} -> true
    | _ -> false

  let is_x87 : C.Type.t -> bool = function
    | `Basic {t=`long_double} -> true
    | _ -> false

  let skip _ = Arg.return ()

  let make_return t k = match t with
    | `Void -> Arg.return ()
    | t ->
      let* size = Arg.size t in
      select (k size) t

  let arena ?low t names = Arg.Arena.of_exps @@
    List.map names ~f:(fun name ->
        match Theory.Target.var t name with
        | None -> failwithf "unknown register: %s" name ()
        | Some reg ->
          let reg = Var.reify reg in
          match low with
          | None -> Bil.var reg
          | Some bits -> Bil.(cast low bits (var reg)))

  let ia16 memory t =
    let data = new C.Size.base `LP32 in
    install t data @@ fun declare ->
    let* irets = arena t ["AX"; "DX"] in
    let return ~alignment:_ _ = choose [
        Arg.registers irets;
        memory;
      ] in
    declare ~return @@ fun ~alignment:_ _ -> memory

  let cdecl16 = ia16 Arg.memory
  let pascal16 = ia16 Arg.push


  let ia32 t k =
    let data = new C.Size.base `ILP32 in
    install t data @@ fun describe ->
    let* irets = arena t ["EAX"; "EDX"] in
    let* frets = arena t ["ST0"] in
    let pass = Arg.memory in
    let return ~alignment:_ size = select [
        C.Type.is_real, Arg.register frets;
        is (size > 64), combine [
          Arg.reference irets;
          Arg.hidden;
        ];
        otherwise, Arg.registers irets;
      ] in
    let* () = Arg.rebase 1 in
    k @@ fun ?(return=return) ?(pass=pass) () ->
    describe ~return @@ fun ~alignment:_ _ -> pass


  (* stdcall, cdecl, watcom-stack, or ms32 *)
  let cdecl t = ia32 t @@ fun accept -> accept ()

  let fastcall t = ia32 t @@ fun override ->
    let is_big size _ = size > 32 in
    let* iregs = arena t ["ECX"; "EDX"] in
    let pass arg =
      let* size = Arg.size arg in
      select [
        any [
          is_big size;
          C.Type.is_floating;
        ], Arg.memory;
        otherwise, choose [
          Arg.register iregs;
          Arg.memory;
        ]
      ] arg in
    override ~pass ()

  let watcomregs t = ia32 t @@ fun override ->
    let* iregs = arena t ["eax"; "edx"; "ebx"; "ecx"] in
    let pass arg = Arg.choice [
        Arg.register iregs arg;
        Arg.memory arg;
      ] in
    override ~pass ()

  (* aka borland register *)
  let pascal t = ia32 t @@ fun override ->
    let* iregs = arena t ["eax"; "edx"; "ecx"] in
    let pass arg = select [
        any C.Type.[is_cint; is_char; is_pointer],
        choose Arg.[register iregs; memory];
      ] arg in
    override ~pass ()


  let ms64 t =
    let data = new C.Size.base `LP64 in
    install t data @@ fun describe ->
    let* iregs = arena t ["rcx"; "rdx"; "r8"; "r9"] in
    let* irets = arena t ["rax"; "rdx"] in
    let* vregs = arena t ~low:64 @@ List.init 4 ~f:(sprintf "ymm%d") in
    let* vrets = arena t ~low:128 ["ymm0"] in
    let iregs = (iregs,[vregs]) and vregs = (vregs,[iregs])
    and irets = (irets,[]) and vrets = (vrets,[]) in
    let use pass (arena,coarena) arg = Arg.sequence [
        pass arena arg;
        Arg.List.iter coarena ~f:Arg.discard
      ] in
    let pass how arena = choose [
        use how arena;
        Arg.memory;
      ] in
    let return ~alignment:_ size = select [
        is_x87, pass Arg.reference iregs;
        C.Type.is_floating, pass Arg.register vrets;
        is (size > 64), pass Arg.reference iregs;
        otherwise, pass Arg.register irets;
      ] in
    describe ~return @@ fun ~alignment:_ size -> select [
      is (size > 64), pass Arg.pointer iregs;
      C.Type.is_floating, pass Arg.register vregs;
      otherwise, pass Arg.register iregs;
    ]

  let merge_kinds k1 k2 = match k1,k2 with
    | `Nil, t | t, `Nil -> t
    | `Int, _ | _, `Int -> `Int
    | `Sse, `Sse -> `Sse

  let partition fields =
    List.fold fields ~init:(`Nil,0,[])
      ~f:(fun (k,s,words) (k',s') ->
          if s + s' <= 64
          then merge_kinds k k',s+s',words
          else k',s',k :: words) |>
    fun (k,_,words) -> List.rev (k::words)

  let rec classify : C.Type.t -> [`Nil | `Int | `Sse] = function
    | t when is_integer t -> `Int
    | t when is_sse t || is_csse t -> `Sse
    | `Structure {t={fields}}
    | `Union {t={fields}} -> classify_fields fields
    | `Array {t={element}} -> classify element
    | _ -> `Nil
  and classify_fields fields =
    List.fold ~init:`Nil fields ~f:(fun k (_,t) ->
        merge_kinds k (classify t))

  let sysv t =
    let data = new C.Size.base `LP64 in
    install t data @@ fun describe ->
    let* iregs = arena t ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"] in
    let* vregs = arena t ~low:64 @@ List.init 8 ~f:(sprintf "ymm%d") in
    let* irets = arena t ["rax"; "rdx"] in
    let* vrets = arena t ~low:64 ["ymm0"; "ymm1"] in

    let union_fields size fields =
      Arg.return [classify_fields fields,size] in
    let rec fields {C.Type.Compound.fields=xs} =
      Arg.List.fold ~init:(0,[]) xs ~f:(fun (off,acc) (_,t) ->
          let alignment = data#alignment t in
          let* size = Arg.size t in
          let size = size + C.Size.padding alignment off in
          let off = off + size in
          match t with
          | t when is_integer t ->
            Arg.return (off, [`Int,size] :: acc)
          | t when is_sse t || is_csse t ->
            Arg.return (off, [`Sse,size] :: acc)
          | `Structure {t} ->
            let+ fields = fields t in
            off, fields :: acc
          | `Union {t={fields}} ->
            let+ fields = union_fields size fields in
            off,fields :: acc
          | `Array {t={C.Type.Array.element; size=Some n}} ->
            let+ size = Arg.size element in
            let kind = classify element in
            off,List.init n ~f:(fun _ -> kind,size) :: acc
          | _ -> Arg.reject ()) >>| fun (_,acc) ->
      List.rev acc |> List.concat in

    let composite_fields : C.Type.t -> _ list Arg.t = function
      | `Structure {t} -> fields t >>| partition
      | `Union {t={fields}} as s ->
        let* size = Arg.size s in
        union_fields size fields >>| partition
      | _ -> Arg.return [] in

    let registers = Arg.registers ~rev:true ~limit:2 in

    let pass_composite memory iregs vregs t =
      Arg.choice [
        composite_fields t >>= begin function
          | [`Int] -> Arg.register iregs t
          | [`Sse] -> Arg.register vregs t
          | [`Int; `Int] -> registers iregs t
          | [`Int; `Sse] -> Arg.split iregs vregs t
          | [`Sse; `Int] -> Arg.split vregs iregs t
          | [`Sse; `Sse] -> registers vregs t
          | _ -> Arg.reject ()
        end;
        memory t;
      ] in

    let return ~alignment:_ bits = select [
        is (bits > 128), Arg.reference iregs;
        is_integer, Arg.register irets;
        is_sse, Arg.register vrets;
        is_csse, registers vrets;
        is_composite, pass_composite (Arg.reference iregs) irets vrets;
      ] in
    describe ~return @@ fun ~alignment:_ bits -> select [
      is (bits > 128), Arg.memory;
      is_integer, Arg.register iregs;
      is_sse, Arg.register vregs;
      is_csse, registers vregs;
      is_composite, pass_composite Arg.memory iregs vregs
    ]


  let calling_conventions = [
    (* 16-bit ABI *)
    i286, [
      Abi.cdecl, cdecl16;
      Abi.pascal, pascal16;
      Abi.fortran, pascal16;
    ];

    (* 32-bit ABI  *)
    i386, [
      Abi.sysv, cdecl;
      Abi.cdecl, cdecl;
      Abi.pascal, pascal;
      Abi.fastcall, fastcall;
      Abi.stdcall, cdecl;
      Abi.ms, cdecl;
    ];

    (* 64-bit ABI *)
    amd64, [
      Abi.ms, ms64;
      Abi.sysv, sysv;
    ]
  ]

  let demanglers = Demangler.[
      Abi.sysv, Theory.Filetype.macho, strip_leading_underscore;
      Abi.ms, Theory.Filetype.coff, strip_leading_underscore;
    ]

  let default_calling_conventions = [
    [i86], cdecl16;
    [i386; i486; i586; i686], cdecl;
    [amd64], sysv;
  ]

  let install_calling_conventions () =
    List.iter calling_conventions ~f:(fun (parent,abis) ->
        List.iter abis ~f:(fun (abi,install) ->
            Theory.Target.filter ~parent ~abi () |>
            List.iter ~f:(fun t ->
                if Theory.Target.bits t = Theory.Target.bits parent
                then install t)));
    List.iter default_calling_conventions ~f:(fun (targets,install) ->
        List.iter targets ~f:install)

  let install_demanglers () =
    List.iter demanglers ~f:(fun (abi,filetype,demangler) ->
        Theory.Target.filter ~parent ~abi ~filetype () |>
        List.iter ~f:(fun target ->
            Demanglers.install target demangler))

  include Abi
end

let subtargets = [
  (* 16-bit targets *)
  [i286],
  Theory.System.[unknown; msdos],
  Theory.Filetype.[unknown],
  Theory.Abi.[cdecl; Abi.pascal; Abi.fortran;];

  (* 32-bit generic targets  *)
  [i686],
  Theory.System.[unknown],
  Theory.Filetype.[coff; aout; elf; macho],
  Theory.Abi.[gnu; cdecl; stdcall; fastcall; watcom; ms];

  (* 64-bit generic targets  *)
  [amd64],
  Theory.System.[unknown],
  Theory.Filetype.[coff; aout; elf; macho],
  Theory.Abi.[gnu; ms];

  (* 32/64 linux/bsd targets  *)
  [i686; amd64],
  Theory.System.[linux; freebsd; openbsd],
  Theory.Filetype.[unknown; aout; coff; elf],
  Theory.Abi.[gnu; cdecl];

  (* 32/64 darwin targets *)
  [i686; amd64],
  Theory.System.[darwin],
  Theory.Filetype.[unknown; macho],
  Theory.Abi.[gnu];

  (* 32-bit windows targets  *)
  [i686],
  Theory.System.[windows],
  Theory.Filetype.[unknown; coff],
  Theory.Abi.[ms; cdecl; fastcall; Abi.pascal; Abi.fortran; watcom];

  (* 64-bit windows targets  *)
  [amd64],
  Theory.System.[windows],
  Theory.Filetype.[unknown; coff],
  Theory.Abi.[ms];

  (* x86 UEFI targets *)
  [i686],
  Theory.System.[uefi],
  Theory.Filetype.[unknown; coff],
  Theory.Abi.[cdecl];

  [amd64],
  Theory.System.[uefi],
  Theory.Filetype.[unknown; coff],
  Theory.Abi.[ms];
]

let register_subtargets () =
  List.iter subtargets ~f:(fun (parents,systems,filetypes,abis) ->
      List.iter parents ~f:(fun parent ->
          Theory.Target.register parent ~systems ~filetypes ~abis))

let enable_loader ~abi () =
  let open KB.Syntax in
  KB.Rule.(declare ~package "x86-target" |>
           require Image.Spec.slot |>
           provide Theory.Unit.target |>
           comment "computes target from the OGRE specification");

  let make_target parent abi' filetype =
    let abi =
      if Theory.Abi.is_unknown abi then abi' else abi in
    Theory.Target.select ~strict:true ~parent ~abi ~filetype () in

  let request =
    let open Ogre.Syntax in
    Ogre.request Image.Scheme.arch >>= fun arch ->
    Ogre.request Image.Scheme.bits >>= fun bits ->
    Ogre.request Image.Scheme.format >>= fun fmt ->
    Ogre.return (arch,bits,fmt) in

  let is_amd64 = function
    | Some ("amd64"|"x86-64"| "x86_64") -> true
    | _ -> false in

  let is_x86 = function
    | Some ("x86"|"i186"|"i286"|"i386"|"i486"|"i586"|"i686") -> true
    | name -> is_amd64 name in

  let get_info doc =
    match Ogre.eval request doc with
    | Error _ -> None,None,None
    | Ok arch -> arch in
  KB.promise Theory.Unit.target @@ fun unit ->
  KB.collect Image.Spec.slot unit >>|
  get_info >>| fun (arch,bits,fmt) ->

  let open Theory.Filetype in

  if is_x86 arch then match bits, fmt with
    | Some 64L, Some "elf" -> make_target amd64 Abi.sysv elf
    | Some 64L, Some "coff" -> make_target amd64 Abi.ms coff
    | Some 64L, Some "macho" -> make_target amd64 Abi.sysv macho
    | Some 32L, Some "elf" -> make_target i686 Abi.sysv elf
    | Some 32L, Some "coff" -> make_target i686 Abi.ms coff
    | Some 32L, Some "macho" -> make_target i686 Abi.sysv macho
    | Some 16L, _ -> make_target i286 abi unknown
    | Some 32L, _ -> make_target i686 abi unknown
    | Some 64L, _ -> make_target amd64 abi unknown
    | _ when is_amd64 arch -> make_target amd64 abi unknown
    | _ -> make_target i686 abi unknown
  else Theory.Target.unknown

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

let unit_encoding =
  KB.Class.property
    ~persistent:Theory.Language.persistent
    ~package:"bap"
    Theory.Unit.cls "unit-encoding" Theory.Language.domain

let compute_unit_encoding backend =
  let open KB.Syntax in
  KB.promise unit_encoding @@ fun unit ->
  let+ t = KB.collect Theory.Unit.target unit in
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

let enable_decoder backend =
  let open KB.Syntax in
  register_x86_llvm_disassembler ();
  register_x86_64_llvm_disassembler ();
  register_sleigh_disassembler ();
  compute_unit_encoding backend;
  KB.promise Theory.Label.encoding @@ fun label ->
  let* unit = label-->?Theory.Label.unit in
  KB.collect unit_encoding unit

let load ?(abi=Theory.Abi.unknown) ?(backend="llvm") () =
  register_subtargets ();
  Abi.install_calling_conventions ();
  Abi.install_demanglers ();
  enable_loader ~abi ();
  enable_arch ();
  enable_decoder backend
