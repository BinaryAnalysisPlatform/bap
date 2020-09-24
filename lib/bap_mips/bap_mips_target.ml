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

let gpr_names = [
  "AT";
  "V0"; "V1";
  "A0"; "A1"; "A2"; "A3";
  "T0"; "T1"; "T2"; "T3"; "T4"; "T5"; "T6"; "T7";
  "S0"; "S1"; "S2"; "S3"; "S4"; "S5"; "S6"; "S7";
  "T8"; "T9";
  "K0"; "K1";
  "GP";
  "SP";
  "FP";
  "RA"
]

let untyped = List.map ~f:Theory.Var.forget
let (@<) xs ys = untyped xs @ untyped ys

let name size order  =
  let order = Theory.Target.Endianness.name order in
  sprintf "mips%d+%s" size (KB.Name.unqualified order)

let parent = Theory.Target.declare ~package "mips"

let define ?(parent=parent) bits endianness =
  let size = Theory.Bitv.size bits in
  let gprs = List.map gpr_names ~f:(reg bits) in
  let mems = Theory.Mem.define bits r8 in
  let data = Theory.Var.define mems "mem" in
  let vars = gprs @< [data] in
  Theory.Target.declare ~package (name size endianness)
    ~parent
    ~bits:size
    ~endianness
    ~vars
    ~code:data
    ~data:data


let mips32bi = define r32 Theory.Target.Endianness.bi
let mips32eb = define r32 Theory.Target.Endianness.eb ~parent:mips32bi
let mips32le = define r32 Theory.Target.Endianness.le ~parent:mips32bi

let mips64bi = define r64 Theory.Target.Endianness.bi
let mips64le = define r64 Theory.Target.Endianness.le ~parent:mips64bi
let mips64eb = define r64 Theory.Target.Endianness.eb ~parent:mips64bi

let enable_loader () =
  KB.Rule.(declare ~package "mips-target" |>
           require Project.specification_slot |>
           provide Theory.Unit.target |>
           comment "computes target from the OGRE specification");
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
  KB.collect Project.specification_slot unit >>|
  request_info >>| function
  | Some "mips", None -> mips32bi
  | Some "mips64",None -> mips64bi
  | Some "mips",Some true -> mips32le
  | Some "mips64",Some true -> mips64le
  | Some "mips",Some false -> mips32eb
  | Some "mips64",Some false -> mips64eb
  | _ -> Theory.Target.unknown


let mapped_mips = Map.of_alist_exn (module Theory.Target) [
    mips32eb, `mips;
    mips32le, `mipsel;
    mips64eb, `mips64;
    mips64le, `mips64el;
  ]

let map_mips () =
  KB.Rule.(declare ~package "mips-arch" |>
           require Theory.Unit.target |>
           provide Arch.unit_slot |>
           comment "computes Arch.t from the unit's target");
  let open KB.Syntax in
  KB.promise Arch.unit_slot @@ fun unit ->
  KB.collect Theory.Unit.target unit >>|
  Map.find mapped_mips >>| function
  | Some arch -> arch
  | None -> `unknown


let load () =
  enable_loader ();
  map_mips ()
