open Core_kernel[@@warning "-D"]
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

let gpr_names = [
  (*  0 *) "ZERO";
  (*  1 *) "AT";
  (*  2 *) "V0"; "V1";
  (*  4 *) "A0"; "A1"; "A2"; "A3";
  (*  8 *) "T0"; "T1"; "T2"; "T3"; "T4"; "T5"; "T6"; "T7";
  (* 16 *) "S0"; "S1"; "S2"; "S3"; "S4"; "S5"; "S6"; "S7";
  (* 24 *) "T8"; "T9";
  (* 26 *) "K0"; "K1";
  (* 28 *) "GP";
  (* 29 *) "SP";
  (* 30 *) "FP";
  (* 31 *) "RA"
]

let untyped = List.map ~f:Theory.Var.forget
let (@<) xs ys = untyped xs @ untyped ys

let name size order  =
  let ends = Theory.Endianness.name order in
  let size = if size = 32 then "" else "64" in
  sprintf "mips%s%s" size @@
  if Theory.Endianness.(equal eb) order then ""
  else if Theory.Endianness.(equal le) order then "el"
  else KB.Name.unqualified ends

let parent = Theory.Target.declare ~package "mips-family"

let array ?(from=0) bits pref n =
  List.init n ~f:(fun i -> reg bits (sprintf "%s%d" pref (i+from)))


let define ?(parent=parent) ?nicknames bits endianness =
  let size = Theory.Bitv.size bits in
  let gprs = array bits "R" 32 in
  let fprs = array bits "F" 32 in
  let mems = Theory.Mem.define bits r8 in
  let data = Theory.Var.define mems "mem" in
  let vars = gprs @< fprs @< [data] in
  let regs = List.map ~f:(fun name -> Theory.Var.forget (reg bits name)) in
  let aliases = List.map gpr_names ~f:(reg bits) in
  let args = if size = 32 then 4 else 8 in
  let iargs = array bits "R" ~from:4 args in
  let fargs = array bits "F" ~from:1 args in
  let aliasing = List.map2_exn gprs aliases ~f:(fun r p ->
      Theory.Alias.(def r [reg p])) in
  Theory.Target.declare ~package (name size endianness)
    ~parent
    ?nicknames
    ~bits:size
    ~endianness
    ~aliasing
    ~code:data
    ~data:data
    ~vars
    ~regs:Theory.Role.Register.[
        [general; integer], regs gpr_names;
        [general; floating], untyped fprs;
        [constant; zero; pseudo], regs ["ZERO"; "R0 "];
        [function_argument; integer],  untyped iargs;
        [function_argument; floating], untyped fargs;
        [function_return; integer],  regs ["R2"];
        [function_return; floating], regs ["F0"];
        [stack_pointer], regs ["R29"];
        [frame_pointer], regs ["R30"];
        [link], regs ["R31"];
        [alias], untyped aliases;
      ]

let mips32bi = define r32 Theory.Endianness.bi
    ~parent
    ~nicknames:["mipsbi"; "mips32bi"]

let mips32eb = define r32 Theory.Endianness.eb
    ~parent
    ~nicknames:["mipseb"; "mips32eb"; "mipsbe"; "mips32be"]

let mips32le = define r32 Theory.Endianness.le
    ~parent
    ~nicknames:["mipsle"; "mipsel"; "mips32le"; "mips32el"]

let mips64bi = define r64 Theory.Endianness.bi
    ~parent
    ~nicknames:["mips64bi"]
let mips64le = define r64 Theory.Endianness.le
    ~parent
    ~nicknames:["misp64le"; "mips64el"]

let mips64eb = define r64 Theory.Endianness.eb
    ~parent
    ~nicknames:["mips64"; "mips64eb"; "mips64be"]

let register_subtargets () =
  List.iter [mips32eb; mips32le; mips64eb; mips64le] ~f:(fun parent ->
      Theory.Target.register parent
        ~abis:Theory.Abi.[unknown; gnu]
        ~systems:Theory.System.[unknown; linux; freebsd; openbsd]
        ~filetypes:Theory.Filetype.[unknown; elf])

let is_mips arch =
  match Option.map ~f:String.lowercase arch with
  | None -> false
  | Some name -> String.is_prefix ~prefix:"mips" name

let is_32bit arch bits = match bits with
  | Some 64L -> false
  | Some 32L -> true
  | _ -> match arch with
    | None -> true
    | Some name ->
      not (String.is_substring ~substring:"64" name)

let enable_loader () =
  KB.Rule.(declare ~package "mips-target" |>
           require Image.Spec.slot |>
           provide Theory.Unit.target |>
           comment "computes a target from the OGRE specification");
  let open KB.Syntax in
  let request_info doc =
    let open Ogre.Syntax in
    let request =
      Ogre.request Image.Scheme.arch >>|
      Option.map ~f:String.lowercase >>= fun arch ->
      Ogre.request Image.Scheme.is_little_endian >>= fun little ->
      Ogre.request Image.Scheme.bits >>= fun bits ->
      Ogre.request Image.Scheme.format >>= fun format ->
      Ogre.return (arch,little,bits,format) in
    match Ogre.eval request doc with
    | Error _ -> None,None,None,None
    | Ok info -> info in
  KB.promise Theory.Unit.target @@ fun unit ->
  KB.collect Image.Spec.slot unit >>| request_info >>|
  fun (arch,is_little,bits,format) ->
  if is_mips arch
  then
    let abi,filetype = match format with
      | Some "elf" -> Theory.Abi.gnu, Theory.Filetype.elf
      | _ -> Theory.Abi.unknown, Theory.Filetype.unknown in
    let parent = match is_32bit arch bits, is_little with
      | true,Some true -> mips32le
      | true,Some false -> mips32eb
      | false,Some true -> mips64le
      | false,Some false -> mips64eb
      | true,None -> mips32bi
      | false,None -> mips64bi in
    Theory.Target.select ~strict:true ~parent ~filetype ~abi ()
  else Theory.Target.unknown

let map_mips () =
  KB.Rule.(declare ~package "mips-arch" |>
           require Theory.Unit.target |>
           provide Arch.unit_slot |>
           comment "computes Arch.t from the unit's target");
  let open KB.Syntax in
  KB.promise Arch.unit_slot @@ fun unit ->
  KB.collect Theory.Unit.target unit >>| fun t ->
  if Theory.Target.belongs parent t
  then List.Assoc.find Theory.Endianness.[
      (32,eb), `mips;
      (32,le), `mipsel;
      (64,eb), `mips64;
      (64,le), `mips64el;
    ] Theory.Target.(bits t, endianness t)
      ~equal:[%equal: int * Theory.Endianness.t] |> function
       | None -> `unknown
       | Some r -> r
  else `unknown

module Dis = Disasm_expert.Basic

let llvm_mips32 = Theory.Language.declare ~package "llvm-mips32"
let llvm_mips64 = Theory.Language.declare ~package "llvm-mips64"
let llvm_mips32le = Theory.Language.declare ~package "llvm-mips32le"
let llvm_mips64le = Theory.Language.declare ~package "llvm-mips64le"

let register encoding triple =
  Dis.register encoding @@ fun _ ->
  Dis.create ~backend:"llvm" triple

let enable_llvm_decoder () =
  let open KB.Syntax in
  register llvm_mips32 "mips";
  register llvm_mips64 "mips64";
  register llvm_mips32le "mipsel";
  register llvm_mips64le "mips64el";
  KB.promise Theory.Label.encoding @@ fun label ->
  Theory.Label.target label >>| fun t ->
  if Theory.Target.belongs parent t then
    let dir = Theory.Target.endianness t in
    match Theory.Target.bits t, Theory.Endianness.(equal dir le) with
    | 32,true  -> llvm_mips32le
    | 64,true  -> llvm_mips64le
    | 32,_ -> llvm_mips32
    | 64,_ -> llvm_mips64
    | _ -> Theory.Language.unknown
  else Theory.Language.unknown

let pcode = Theory.Language.declare ~package:"bap" "pcode-mips"

let is_big t = Theory.Endianness.(Theory.Target.endianness t = eb)

let register_ghidra_backend () =
  Dis.register pcode @@ fun t ->
  let triple =
    match Theory.Target.bits t = 32, is_big t with
    | true,true  -> "MIPS:BE:32:default"
    | true,false -> "MIPS:LE:32:default"
    | false,true -> "MIPS:BE:64:default"
    | false,false -> "MIPS:LE:64:default" in
  Dis.create ~backend:"ghidra" triple

let enable_pcode_decoder () =
  register_ghidra_backend ();
  let open KB.Syntax in
  KB.promise Theory.Label.encoding @@ fun label ->
  Theory.Label.target label >>| fun t ->
  if Theory.Target.belongs parent t
  then pcode
  else Theory.Language.unknown

let load ?(backend="llvm") () =
  register_subtargets ();
  enable_loader ();
  map_mips ();
  match backend with
  | "llvm" -> enable_llvm_decoder ()
  | "ghidra" -> enable_pcode_decoder ()
  | s -> invalid_argf "unknown backend %S, expected %S or %S"
           s "llvm" "ghidra" ()
