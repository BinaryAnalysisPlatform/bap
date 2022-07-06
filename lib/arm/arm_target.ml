let package = "bap"

open Core_kernel[@@warning "-D"]
open Bap_core_theory
open Bap.Std
open KB.Syntax
open Poly
open Bap_traces.Std

module CT = Theory

type r128 and r80 and r64 and r32 and r16 and r8

type 'a bitv = 'a CT.Bitv.t CT.Value.sort

let r128 : r128 bitv = CT.Bitv.define 128
let r80 : r80 bitv = CT.Bitv.define 80
let r64 : r64 bitv = CT.Bitv.define 64
let r32 : r32 bitv = CT.Bitv.define 32
let r16 : r16 bitv = CT.Bitv.define 16
let r8  : r8  bitv = CT.Bitv.define 8
let bool = CT.Bool.t


let reg t n = CT.Var.define t n
let untyped = List.map ~f:CT.Var.forget
let (@<) xs ys = untyped xs @ untyped ys

let array ?(index=string_of_int) t pref size =
  List.init size ~f:(fun i -> reg t (pref ^ index i))

let mems = CT.Mem.define r32 r8
let data = CT.Var.define mems (Var.name Arm_env.mem)

let of_bil v =
  CT.Var.define (Var.sort v) (Var.name v)

let regs xs = untyped@@List.map ~f:of_bil xs

let vars32 = regs Arm_env.[
    r0; r1; r2; r3; r4; r5; r6; r7; r8; r9;
    r10; r11; r12; sp; lr; mem;
    nf; zf; cf; vf; qf;
  ]


let thumb = Theory.Role.declare ~package:"arm" "thumb"

let status_regs = Theory.Role.Register.[
    [status; integer], regs Arm_env.[nf; zf; cf; vf; qf];
    [carry_flag], regs Arm_env.[cf];
    [sign_flag], regs Arm_env.[nf];
    [zero_flag], regs Arm_env.[zf];
    [overflow_flag], regs Arm_env.[vf];
  ]

(* AACPS, §5.1.1 Core registers:

   - The first four registers r0-r3 (a1-a4) are used to pass
     argument values into a subroutine and to return a result
     value from a function. They may also be used to hold intermediate
     values within a routine (but, in general, only between subroutine
     calls).

   - Register r12 (IP) may be used by a linker as a scratch register
     between a routine and any subroutine it calls (for details,
     see §5.3.1.1, Use of IP by the linker). It can also be used within
     a routine to hold intermediate values between subroutine calls.

   - The role of register r9 is platform specific. A virtual platform
     may assign any role to this register and must document this usage.
     For example, it may designate it as the static base (SB) in a
     position-independent data model, or it may designate it as the thread
     register (TR) in an environment with thread-local storage. The usage
     of this register may require that the value held is persistent across
     all calls. A virtual platform that has no need for such a special
     register may designate r9 as an additional callee-saved variable
     register, v6.

   - A subroutine must preserve the contents of the registers
     r4-r8, r10, r11 and SP (and r9 in PCS variants that designate
     r9 as v6).
*)
let regs32 = Theory.Role.Register.[
    [general; integer], regs Arm_env.[
        r0; r1; r2; r3; r4; r5; r6; r7; r8; r9; r10; r11; r12;
        sp; lr;
      ];
    [stack_pointer], regs Arm_env.[sp];
    [frame_pointer], regs Arm_env.[r11];
    [link], regs Arm_env.[lr];
    [thumb], regs Arm_env.[
        r0; r1; r2; r3; r4; r5; r6; r7; sp; lr;
      ];
    [function_argument], regs Arm_env.[r0; r1; r2; r3];
    [function_return], regs Arm_env.[r0; r1];
    [caller_saved], regs Arm_env.[
        r0; r1; r2; r3; r12; lr;
      ];
    [callee_saved], regs Arm_env.[
        r4; r5; r6; r7; r8; r9; r10; r11; sp;
      ];
  ] @ status_regs

let vfp2regs = Theory.Role.Register.[
    [general; floating], untyped@@array r64 "D" 16;
  ]

let vfp3regs = Theory.Role.Register.[
    [general; floating], untyped@@array r64 "D" 32;
  ]

let vars32_fp = vars32 @ untyped @@ array r64 "D" 16

let rs = array r64 "R" 32
let xs = array r64 "X" 32
let ws = array r32 "W" 32
let vs = array r128 "V" 32
let qs = array r128 "Q" 32
let ds = array r64 "D" 32
let ss = array r32 "S" 32
let hs = array r16 "H" 32
let bs = array r8 "B" 32
let fp64 = reg r64 "FP"       (* X29 *)
let lr64 = reg r64 "LR"       (* X30 *)
let sp64 = reg r64 "SP"       (* X31 *)
let sp32 = reg r32 "WSP"      (* W31 *)
let zr = reg r64 "ZR"
let zr64 = reg r64 "XZR"
let zr32 = reg r32 "WZR"
let memsv8 = CT.Mem.define r64 r8
let datav8 = CT.Var.define memsv8 "mem"
let flagsv8 = [
  reg bool "NF";
  reg bool "ZF";
  reg bool "CF";
  reg bool "VF";
]

let (.$()) = List.nth_exn

let aliases =
  xs @< ws @< qs @< ds @< ss @< hs @< bs
  @<[fp64; lr64; sp64; zr64]
  @<[sp32; zr32]

let varsv8 = rs @< flagsv8 @< [datav8]

let regsv8 = Theory.Role.Register.[
    [general; integer], untyped rs;
    [general; floating], untyped xs;
    [stack_pointer], untyped [reg r64 "R31"];
    [frame_pointer], untyped [reg r64 "R29"];
    [function_argument; function_return],
    array r64 "R" 8 @< array r64 "V" 8;
    [constant; zero; pseudo], untyped [reg r64 "XZR"; reg r64 "ZR"];
    [constant; zero; pseudo], untyped [reg r32 "WZR"];
    [link], untyped [reg r64 "R30"];
    [alias], aliases;
  ] @ status_regs

let equal xs ys =
  List.map2_exn xs ys ~f:Theory.Alias.(fun x y -> def x [reg y])

let lower xs _ ys =
  List.map2_exn xs ys ~f:Theory.Alias.(fun x y -> def x [unk; reg y])

let are f x y = f x y

let aliasing = Theory.Alias.[
    [
      def fp64 [reg xs.$(29)];
      def lr64 [reg xs.$(30)];
      def sp64 [reg xs.$(31)];
      def sp64 [unk; reg sp32];
      def zr [reg zr64];
      def zr [unk; reg zr32];
    ];
    are equal rs xs;
    lower rs are ws;
    are equal vs qs;
    lower qs are ds;
    lower ds are ss;
    lower ss are hs;
    lower hs are bs;
  ] |> List.concat


let parent = CT.Target.declare ~package "arm-family"
let is_arm = CT.Target.belongs parent
let is_64bit t = Theory.Target.bits t = 64
let is_big t = Theory.Endianness.(equal eb (Theory.Target.endianness t))
let is_little t = Theory.Target.endianness t = Theory.Endianness.le

module type ARM = sig
  val endianness : CT.endianness
  val parent : CT.Target.t
  val aarch32 : Theory.Target.t
  val aarch64 : Theory.Target.t
  val v4 : CT.Target.t
  val v4t : CT.Target.t
  val v5 : CT.Target.t
  val v5t : CT.Target.t
  val v5te : CT.Target.t
  val v5tej : CT.Target.t
  val v6 : CT.Target.t
  val v6t2 : CT.Target.t
  val v6z : CT.Target.t
  val v6k : CT.Target.t
  val v6m : CT.Target.t
  val v7 : CT.Target.t
  val v7fp : CT.Target.t
  val v7a : CT.Target.t
  val v7afp : CT.Target.t
  val v7m : CT.Target.t
  val v8a : CT.Target.t
  val v81a : CT.Target.t
  val v82a : CT.Target.t
  val v83a : CT.Target.t
  val v84a : CT.Target.t
  val v85a : CT.Target.t
  val v86a : CT.Target.t
end

module type Endianness =  sig val endianness : CT.endianness end
module Family (Order : Endianness) = struct
  include Order

  let is_little = Theory.Endianness.(equal endianness le)
  let is_bi_endian = CT.Endianness.(equal bi) endianness

  let ordered name =
    let order = CT.Endianness.name endianness in
    name ^ "+" ^ KB.Name.unqualified order

  let def ?code_alignment ~parent name =
    if CT.Target.is_unknown parent
    then CT.Target.unknown
    else CT.Target.declare ~package (ordered name) ~parent
        ?code_alignment
        ~nicknames:[name]

  let (<:) parent name = def ~parent name


  let v4 =
    if is_bi_endian
    then CT.Target.unknown
    else CT.Target.declare ~package (ordered "armv4")
        ~parent
        ~code_alignment:32
        ~nicknames:["armv4"]
        ~bits:32
        ~byte:8
        ~endianness
        ~code:data
        ~data:data
        ~vars:vars32
        ~regs:regs32

  let v4t = def "armv4t" ~parent:v4 ~code_alignment:16
  let v5    = v4 <: "armv5"
  let v5t   = v5 <: "armv5t"
  let v5te  = v5t <: "armv5te"
  let v5tej = v5te <: "armv5tej"
  let v6    = v5tej <: "armv6"
  let v6t2  = v6 <: "armv6t2"
  let v6z   = v6 <: "armv6z"
  let v6k   = v6z <: "armv6k"
  let v6m   = v6 <: "armv6-m"

  let v7 = if not is_bi_endian then v6t2 <: "armv7"
    else CT.Target.declare ~package (ordered "armv7")
        ~parent
        ~nicknames:["armv7"]
        ~code_alignment:16
        ~bits:32
        ~byte:8
        ~endianness
        ~code:data
        ~data:data
        ~vars:vars32
        ~regs:regs32

  let v7m = v7 <: "armv7-m"

  let v7fp  = CT.Target.declare ~package (ordered "armv7+fp") ~parent:v7
      ~nicknames:["armv7+fp"]
      ~vars:vars32_fp
      ~regs:(regs32@vfp3regs)

  let v7a    = v7 <: "armv7-a"
  let v7afp  = CT.Target.declare ~package (ordered "armv7-a+fp")
      ~nicknames:["armv7-a+fp"]
      ~parent:v7a
      ~vars:vars32_fp
      ~regs:(regs32@vfp3regs)

  (* the generic final v7 incorporating all 32-bit targets *)
  let aarch32 =
    if is_bi_endian then Theory.Target.unknown
    else
      Theory.Target.declare ~package
        (if is_little then "arm" else "armeb")
        ~parent:v7afp
        ~nicknames:[
          if is_little
          then "aarch32"
          else "aarch32eb";
        ]


  let v8 =
    CT.Target.declare ~package (ordered "armv8")
      ~parent:(if is_bi_endian then parent else aarch32)
      ~nicknames:["aarch64"]
      ~aliasing
      ~code_alignment:32
      ~bits:64
      ~code:datav8
      ~data:datav8
      ~vars:varsv8
      ~regs:regsv8


  let v8a =
    CT.Target.declare ~package (ordered "armv8-a") ~parent:v8
      ~nicknames:["armv8-a"]

  let v8a32 =
    Theory.Target.declare ~package (ordered "armv8-a+aarch32")
      ~nicknames:["armv8-a+aarch32"]
      ~parent:v7

  let v8m32 =
    Theory.Target.declare ~package (ordered "armv8-m+aarch32")
      ~nicknames:["armv8-m+aarch32"]
      ~parent:v7m

  let v8r32 =
    Theory.Target.declare ~package (ordered "armv8-r+aarch32")
      ~nicknames:["armv8-r+aarch32"]
      ~parent:v7

  let v81a = v8a  <: "armv8.1-a"
  let v82a = v81a <: "armv8.2-a"
  let v83a = v82a <: "armv8.3-a"
  let v84a = v83a <: "armv8.4-a"
  let v85a = v84a <: "armv8.5-a"
  let v86a = v85a <: "armv8.6-a"

  (* the generic final v8 incorporating all 32-bit targets *)
  let aarch64 =
    if is_bi_endian then Theory.Target.unknown
    else
      Theory.Target.declare ~package
        (if is_little then "aarch64" else "aarch64_be")
        ~parent:v86a
        ~nicknames:[
          if is_little
          then "arm64"
          else "arm64eb";
        ]

  let v9a = v86a <: "armv9-a"

  let parent = if is_bi_endian then v7 else v4

end

module LE = Family(struct let endianness = CT.Endianness.le end)
module Bi = Family(struct let endianness = CT.Endianness.bi end)
module EB = Family(struct let endianness = CT.Endianness.eb end)

let family_of_endian is_little : (module ARM) = match is_little with
  | None -> (module Bi)
  | Some true -> (module LE)
  | Some false -> (module EB)


let prefixes = ["arm"; "thumb"; "aarch64";]
let suffixes = ["eb"; "_be"]

let in_family = function
  | None -> false
  | Some x -> List.exists prefixes ~f:(fun prefix ->
      String.is_prefix ~prefix x)

let drop_end s =
  Option.value ~default:s @@
  List.find_map suffixes ~f:(fun suffix ->
      String.chop_suffix ~suffix s)

let split s = List.find_map_exn prefixes ~f:(fun prefix ->
    match String.chop_prefix ~prefix s with
    | None -> None
    | Some r -> Some (prefix,drop_end r))

let normalize arch sub =
  match arch,sub with
  | None,_ -> assert false
  | Some arch,None -> split arch
  | Some arch, Some sub -> arch,sub

let enable_loader () =
  let open Bap.Std in
  KB.Rule.(declare ~package "arm-target" |>
           require Image.Spec.slot |>
           provide CT.Unit.target |>
           comment "computes target from the OGRE specification");
  let open KB.Syntax in
  let request_info doc =
    let open Ogre.Syntax in
    let request =
      Ogre.request Image.Scheme.arch >>= fun arch ->
      Ogre.request Image.Scheme.subarch >>= fun sub ->
      Ogre.request Image.Scheme.is_little_endian >>= fun little ->
      Ogre.request Image.Scheme.format >>= fun filetype ->
      Ogre.return (arch,sub,little,filetype) in
    match Ogre.eval request doc with
    | Error _ -> None,None,None,None
    | Ok info -> info in
  KB.promise CT.Unit.target @@ fun unit ->
  KB.collect Image.Spec.slot unit >>|
  request_info >>| fun (arch,sub,is_little,filetype) ->
  if not (in_family arch) then CT.Target.unknown
  else
    let module Family = (val family_of_endian is_little) in
    let parent = match normalize arch sub with
      | "arm","v6m" -> Family.v6m
      | "thumb",_   -> Family.v7m
      | "arm",
        ("v4"|"v4t"|
         "v5"|"v5t"|"v5te"|"v5tej"|
         "v6"|"v6z"|"v6k"|"v6t2"|
         "v7"|"v7fp"|"v7a"|"v7afp") -> Family.aarch32
      | "arm",
        ("v8"|"v8a"|"v81a"|"v82a"|"v83a"|"v84a"|"v85a"|"v86a") ->
        Family.aarch64
      | "aarch64",_   -> Family.aarch64
      | _ -> Family.v7 in
    let is_64bit = Theory.Target.bits parent = 64 in
    let filetype,system,abi = match filetype with
      | Some "elf" -> Theory.Filetype.elf,
                      Theory.System.linux,
                      if is_64bit then Theory.Abi.gnu else Theory.Abi.gnueabi
      | Some "coff" -> Theory.Filetype.coff,Theory.System.windows,Theory.Abi.eabi
      | Some "macho" -> Theory.Filetype.macho, Theory.System.darwin,Theory.Abi.eabi
      | _ -> Theory.Filetype.unknown,Theory.System.unknown,Theory.Abi.unknown in
    Theory.Target.select ~strict:true ~system ~parent ~filetype ~abi ()


type arms = [
  | Arch.arm
  | Arch.armeb
  | Arch.thumb
  | Arch.thumbeb
  | Arch.aarch64
]

let arms : arms Map.M(CT.Target).t =
  Map.of_alist_exn (module CT.Target) [
    LE.aarch32, `armv7;
    EB.aarch32, `armv7eb;
    LE.aarch64, `aarch64;
    EB.aarch64, `aarch64_be;
    LE.v4, `armv4;
    LE.v4t, `armv4;
    LE.v5, `armv5;
    LE.v5t, `armv5;
    LE.v5te, `armv5;
    LE.v5tej, `armv5;
    LE.v6, `armv6;
    LE.v6z, `armv6;
    LE.v6k, `armv6;
    LE.v6m, `armv6;
    LE.v6t2, `armv6;
    LE.v7, `armv7;
    LE.v7a, `armv7;
    LE.v7m, `thumbv7;
    LE.v7afp, `armv7;
    Bi.v7, `armv7;
    Bi.v7a, `armv7;
    Bi.v7m, `thumbv7;
    Bi.v7afp, `armv7;
    LE.v8a, `aarch64;
    LE.v81a, `aarch64;
    LE.v82a, `aarch64;
    LE.v83a, `aarch64;
    LE.v84a, `aarch64;
    LE.v85a, `aarch64;
    LE.v86a, `aarch64;
    EB.v4, `armv4eb;
    EB.v4t, `armv4eb;
    EB.v5, `armv5eb;
    EB.v5t, `armv5eb;
    EB.v5te, `armv5eb;
    EB.v5tej, `armv5eb;
    EB.v6,  `armv6eb;
    EB.v6z, `armv6eb;
    EB.v6k, `armv6eb;
    EB.v6m, `armv6eb;
    EB.v6t2,`armv6eb;
    EB.v7, `armv7eb;
    EB.v7a, `armv7eb;
    EB.v7m, `thumbv7eb;
    EB.v7afp, `armv7eb;
    EB.v8a,  `aarch64_be;
    EB.v81a, `aarch64_be;
    EB.v82a, `aarch64_be;
    EB.v83a, `aarch64_be;
    EB.v84a, `aarch64_be;
    EB.v85a, `aarch64_be;
    EB.v86a, `aarch64_be;
  ]


let smc = Theory.Abi.declare ~package:"arm" "smc"
let hvc = Theory.Abi.declare ~package:"arm" "hvc"

let subtargets = [
  (*  32-bit targets *)

  (* generic and standalone targets *)
  [EB.aarch32; LE.aarch32],
  Theory.System.[unknown; none],
  Theory.Abi.[eabi],
  Theory.Fabi.[unknown; hard],
  Theory.Filetype.[unknown];

  (* uefi  *)
  [LE.aarch32],
  Theory.System.[uefi],
  Theory.Abi.[smc],
  Theory.Fabi.[unknown],
  Theory.Filetype.[unknown; coff];

  (* linux targets *)
  [EB.aarch32; LE.aarch32],
  Theory.System.[linux],
  Theory.Abi.[gnueabi; gnu],
  Theory.Fabi.[unknown; hard],
  Theory.Filetype.[unknown; elf];

  (* darwin targets *)
  [LE.aarch32],
  Theory.System.[darwin],
  Theory.Abi.[eabi],
  Theory.Fabi.[unknown],
  Theory.Filetype.[unknown; macho];

  (* 64-bit targets  *)
  [EB.aarch64; LE.aarch64],
  Theory.System.[unknown; none],
  Theory.Abi.[eabi],
  Theory.Fabi.[unknown],
  Theory.Filetype.[unknown];

  (* uefi  *)
  [LE.aarch64],
  Theory.System.[uefi],
  Theory.Abi.[smc],
  Theory.Fabi.[unknown],
  Theory.Filetype.[unknown; coff];

  (* linux targets *)
  [EB.aarch64; LE.aarch64],
  Theory.System.[linux],
  Theory.Abi.[gnu],
  Theory.Fabi.[unknown],
  Theory.Filetype.[unknown; elf];

  (* darwin targets *)
  [LE.aarch64],
  Theory.System.[darwin],
  Theory.Abi.[eabi],
  Theory.Fabi.[unknown],
  Theory.Filetype.[unknown; macho];

  (* windows targets *)
  [LE.aarch64],
  Theory.System.[windows],
  Theory.Abi.[eabi],
  Theory.Fabi.[unknown],
  Theory.Filetype.[unknown; coff];
]

let install_subtargets () =
  List.iter subtargets ~f:(fun (family,systems,abis,fabis,filetypes) ->
      List.iter family ~f:(Theory.Target.register ~systems ~abis ~fabis ~filetypes))

let enable_arch () =
  let open KB.Syntax in
  KB.Rule.(declare ~package "arm-arch" |>
           require CT.Unit.target |>
           provide Arch.unit_slot |>
           comment "computes Arch.t from the unit's target");
  KB.promise Arch.unit_slot @@ fun unit ->
  KB.collect CT.Unit.target unit >>| fun t ->
  if is_arm t then match Map.find arms t with
    | Some arch -> (arch :> Arch.t)
    | None -> match is_64bit t,is_big t,is_little t with
      | _,false,false -> `unknown
      | true,true,_ -> `aarch64_be
      | true,false,_ -> `aarch64
      | false,true,_ -> `armv7eb
      | false,false,_ -> `armv7
  else `unknown

let llvm_a32 = CT.Language.declare ~package "llvm-armv7"
let llvm_t32 = CT.Language.declare ~package "llvm-thumb"
let llvm_a64 = CT.Language.declare ~package "llvm-aarch64"
let pcode = CT.Language.declare ~package "pcode-arm"

module Dis = Disasm_expert.Basic

let register ?attrs encoding triple =
  Dis.register encoding @@ fun t ->
  let triple = if Theory.Endianness.(eb = Theory.Target.endianness t)
    then triple ^ "eb" else triple in
  Dis.create ?attrs ~backend:"llvm" triple

let symbol_values doc =
  let open Ogre.Let in
  let open Image.Scheme in
  let symbols =
    let* symtab =
      Ogre.(collect Query.(select (from symbol_value))) in
    let+ entry = Ogre.request entry_point in
    match entry with
    | None -> symtab
    | Some entry ->
      let mask = Int64.(-1L lsl 1) in
      Seq.cons Int64.(entry land mask, entry) symtab in
  match Ogre.eval symbols doc with
  | Ok syms -> syms
  | Error err ->
    failwithf "Arm_target: broken file specification: %s"
      (Error.to_string_hum err) ()

module Encodings = struct
  open Bap_primus.Std
  module Lambda = Primus.Lisp
  module Sigma = Lambda.Semantics

  let empty = Map.empty (module Bitvec_order)

  let lsb x = Int64.(x land 1L)
  let is_thumb x = Int64.equal (lsb x) 1L


  let symbols_encoding spec =
    symbol_values spec |>
    Seq.fold ~init:empty ~f:(fun symbols (addr,value) ->
        let addr = Bitvec.M32.int64 addr in
        if is_thumb value
        then Map.set symbols addr llvm_t32
        else Map.update symbols addr ~f:(function
            | None -> llvm_a32
            | Some t -> t))

  let slot = KB.Class.property CT.Unit.cls
      ~package "symbols-encodings" @@
    KB.Domain.mapping (module Bitvec_order) "encodings"
      ~equal:CT.Language.equal


  let set_encoding label x y =
    let* addr = x.?[Sigma.static] in
    let* code = y.?[Sigma.symbol] in
    let* unit = label-->?Theory.Label.unit in
    let* lang = match code with
      | ":t32" | ":T32" -> !!llvm_t32
      | ":a32" | ":A32" -> !!llvm_a32
      | other ->
        Sigma.failp "unknown encoding %s, expects :T32 or :A32" other in
    let* encodings = unit-->slot in
    let res = Map.set encodings addr lang in
    KB.catch (KB.provide slot unit res) (fun _ -> KB.return ()) >>|
    fun () -> Sigma.Effect.pure Sigma.Value.nil


  let provide_primitive () =
    let types = Lambda.Type.Spec.(tuple [int; sym] @-> sym) in
    let docs = "(arm-set-encoding ADDR ENC) specifies the encoding \
                of instruction at ADDR as ENC, where ENC is either, \
                :T32 or :A32" in
    Sigma.declare ~types ~docs ~package:"bap" "arm-set-encoding"
      ~body:(fun _ -> KB.return @@ fun lbl args -> match args with
        | [addr; encoding] -> set_encoding lbl addr encoding
        | _ -> Sigma.failp "expected two arguments")

  let provide_encodings () =
    let open KB.Syntax in
    KB.promise slot @@ fun label ->
    KB.collect Image.Spec.slot label >>|
    symbols_encoding

  let provide () =
    provide_encodings ();
    provide_primitive ()
end

module Modes = struct
  let a32 = Mode.declare ~package:"arm" "a32"
  let t32 = Mode.declare ~package:"arm" "t32"
end

let has_t32 label =
  KB.collect CT.Label.unit label >>= function
  | None -> !!false
  | Some unit ->
    KB.collect Encodings.slot unit >>|
    Map.exists ~f:(Theory.Language.equal llvm_t32)


let is_word_aligned x = Bitvec.(M32.(int 3 land x) = zero)

let compute_encoding_from_symbol_table label =
  let (>>=?) x f = x >>= function
    | None -> !!Theory.Language.unknown
    | Some x -> f x in
  KB.collect CT.Label.unit label >>=? fun unit ->
  KB.collect CT.Label.addr label >>=? fun addr ->
  KB.collect Encodings.slot unit >>| fun encodings ->
  if not (is_word_aligned addr) then llvm_t32
  else match Map.find encodings addr with
    | Some x -> x
    | None -> CT.Language.unknown

(* here t < p means that t was introduced before p *)
let (>=) t p = CT.Target.belongs t p
let (<) t p = t >= p && not (p >= t)
let (<=) t p = t = p || t < p

let m_profiles = [
  LE.v7m; EB.v7m; Bi.v7m;
  LE.v8m32; EB.v8m32; Bi.v8m32;
]
let is_thumb_only t =
  List.exists m_profiles ~f:(fun p -> p <= t)


let register_pcode () =
  Dis.register pcode @@ fun t ->
  let triple = match is_64bit t,is_little t,is_big t with
    | true,true,_ -> "AARCH64:LE:64:v8A"
    | true,_,_ -> "AARCH64:BE:64:v8A"
    | false,true,_ -> "ARM:LE:32:v7"
    | false,_,true -> "ARM:BE:32:v7"
    | false,_,_    -> "ARM:LEBE:32:v7LEInstruction" in
  Dis.create ~backend:"ghidra" triple

let enable_pcode () =
  register_pcode ();
  KB.promise Theory.Label.encoding @@ fun label ->
  Theory.Label.target label >>| fun t ->
  if is_arm t then pcode
  else Theory.Language.unknown

let guess_encoding interworking label target mode =
  if is_arm target then
    if is_64bit target then !!llvm_a64 else
    if is_thumb_only target
    then !!llvm_t32
    else
      let from_mode_or fallback =
        if Mode.equal mode Modes.t32 then !!llvm_t32
        else if Mode.equal mode Modes.a32 then !!llvm_a32
        else fallback () in
      match interworking with
      | Some true -> from_mode_or @@ fun () ->
        compute_encoding_from_symbol_table label
      | Some false -> !!llvm_a32
      | None -> from_mode_or @@ fun () ->
        has_t32 label >>= function
        | true -> compute_encoding_from_symbol_table label
        | false -> !!llvm_a32
  else !!CT.Language.unknown

(* our lowest supported llvm is 6.0 which supports up to v8.3a *)
let max_v8a_version = 3

let v8aversions =
  List.init max_v8a_version ~f:(fun i ->
      sprintf "+v8.%da" (i+1))

let is_normal_feature s =
  String.length s > 0 && match s.[0] with
  | '+' | '-' -> true
  | _ -> false

let normalize_features xs =
  List.map xs ~f:(fun x ->
      if is_normal_feature x
      then x
      else "+" ^ x) |>
  String.concat ~sep:","

let enable_llvm ?(features=[]) ?interworking () =
  let open KB.Syntax in
  let features xs = normalize_features (xs@features) in
  register llvm_a32 "armv7" ~attrs:(features []);
  register llvm_t32 "thumbv7" ~attrs:(features ["thumb2"]);
  register llvm_a64 "aarch64" ~attrs:(features v8aversions);
  KB.promise CT.Label.encoding @@ fun label ->
  let* target = CT.Label.target label in
  let* mode = KB.collect Mode.slot label in
  guess_encoding interworking label target mode

let load ?features ?interworking ?(backend="llvm") () =
  install_subtargets ();
  enable_loader ();
  enable_arch ();
  Encodings.provide ();
  if String.equal backend "llvm"
  then enable_llvm ?features ?interworking ()
  else enable_pcode ()
