open Core_kernel.Std
open Regular.Std
open Bap.Std
open Bap_api

type t = {
  arch : arch;
  name : string;
} [@@deriving bin_io, compare, sexp]

module Abi = Regular.Make(struct
    type nonrec t = t [@@deriving bin_io, compare, sexp]
    let version = "0.1"
    let module_name = Some "Bap_api_abi"
    let hash = Hashtbl.hash
    let pp ppf {arch;name} =
      Format.fprintf ppf "%a-%s" Arch.pp arch name
  end)

type resolver = string -> attr list -> string

let abis : (pos -> exp) Abi.Table.t = Abi.Table.create ()
let resolvers : resolver Arch.Table.t = Arch.Table.create ()
let register arch name f = Hashtbl.set abis ~key:{arch;name} ~data:f
let override_resolver arch f = Hashtbl.set resolvers ~key:arch ~data:f

let resolve arch attr name = match Hashtbl.find resolvers arch with
  | Some f -> {arch; name = f attr name}
  | None -> {arch; name = "unknown"}

let apply abi pos = match Hashtbl.find abis abi with
  | Some abi -> abi pos
  | None ->
    let width = Arch.addr_size abi.arch |> Size.in_bits in
    Bil.unknown "unknown abi" (Type.Imm width)

let known_abi arch =
  Hashtbl.keys abis|> List.filter_map ~f:(fun abi ->
      Option.some_if (abi.arch = arch) abi.name)
