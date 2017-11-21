open Core_kernel.Std
open Bap.Std

type cast  = Bil.cast  [@@deriving bin_io, compare, sexp]
type binop = Bil.binop [@@deriving bin_io, compare, sexp]
type unop  = Bil.unop  [@@deriving bin_io, compare, sexp]
type e   = Bil.exp   [@@deriving bin_io, compare, sexp]
type rtl = Bil.stmt  [@@deriving bin_io, compare, sexp]

module Infix = Bil.Infix

let cast = Bil.cast
let var = Bil.var
let unsigned = Bil.unsigned
let signed = Bil.signed
let high = Bil.high
let low = Bil.low

let int = Bil.int
let extract = Bil.extract
let concat = Bil.concat
let if_ = Bil.if_
let jmp = Bil.jmp

open Powerpc_model
open Hardware

let bil = ident

let powerpc_fail format =
  let fail str = failwith (sprintf "PowerPC lifter fail: %s" str) in
  Printf.ksprintf fail format

let load addr_size ~addr endian size = match addr_size with
  | `r32 -> Bil.(load ~mem:(var PowerPC_32.mem) ~addr endian size)
  | `r64 -> Bil.(load ~mem:(var PowerPC_64.mem) ~addr endian size)

let store addr_size ~addr endian size data =
  match addr_size with
  | `r32 ->
    Bil.(PowerPC_32.mem := store ~mem:(var PowerPC_32.mem) ~addr data endian size)
  | `r64 ->
    Bil.(PowerPC_64.mem := store ~mem:(var PowerPC_64.mem) ~addr data endian size)

let fresh name typ = Var.create ~fresh:true name typ

let low32 exp = Bil.extract 31 0 exp

let is_negative mode exp = match mode with
  | `r32 -> Bil.(low32 exp <$ int @@ Word.zero 32)
  | `r64 -> Bil.(exp <$ int @@ Word.zero 64)

let is_positive mode exp = match mode with
  | `r32 -> Bil.(low32 exp >$ int @@ Word.zero 32)
  | `r64 -> Bil.(exp >$ int @@ Word.zero 64)

let is_zero mode exp = match mode with
  | `r32 -> Bil.(low32 exp = int @@ Word.zero 32)
  | `r64 -> Bil.(exp = int @@ Word.zero 64)

let is_var_named name var = String.equal name (Var.name var)

(** will also try to find R# when got X#, (e.g. R3 when got X3)
    for reasons depended only from llvm side *)
let find_gpr reg =
  let find name = Var.Set.find ~f:(is_var_named name) gpr in
  let reg_name = Reg.name reg in
  match find reg_name with
  | Some r -> Some r
  | None ->
    if String.is_prefix reg_name ~prefix:"X" then
      let name = String.substr_replace_first
          reg_name ~pattern:"X" ~with_:"R" in
      find name
    else None

let find_cr_bit reg =
  let name = Reg.name reg in
  Int.Map.data cr |>
  List.find ~f:(fun v -> String.equal (Var.name v) name)

let reg_searches = [find_gpr; find_cr_bit;]

let find reg =
  List.filter_map reg_searches ~f:(fun f -> f reg) |> function
  | [] -> powerpc_fail "Register not found: %s" (Reg.name reg)
  | hd :: [] -> hd
  | _ -> powerpc_fail "Register name %s is ambigous!!!" (Reg.name reg)

let exists reg =
  List.exists reg_searches ~f:(fun f -> Option.is_some (f reg))

let cr_bit n =
  let n = cr_bitwidth - n - 1 in
  match Int.Map.find cr n with
  | Some b -> b
  | None -> powerpc_fail "CR bit number %d does not found" n

let cr_field reg =
  let name = Reg.name reg in
  String.Map.find cr_fields name |> function
  | None -> powerpc_fail "CR fields %s does not found" name
  | Some (w,x,y,z) -> z,y,x,w

include Infix
