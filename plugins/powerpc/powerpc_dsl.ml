open Core_kernel
open Bap.Std
open Regular.Std

open Powerpc_rtl
open Powerpc_utils

type 'a ec = bool -> 'a

type bitwidth = int

let bit = 1
let byte = 8
let halfword = 16
let word = 32
let doubleword = 64
let quadword = 128
let bitwidth x = x

let int_of_bitwidth = ident

let width_of_size = Size.in_bits

let int_of_imm = function
  | Op.Reg _ | Op.Fmm _ -> ppc_fail "imm operand expected"
  | Op.Imm x -> match Imm.to_int x with
    | Some x -> x
    | None -> ppc_fail "failed to convert imm operand to int"

let imm signed op =
  let w = Word.of_int ~width:32 (int_of_imm op) in
  if signed then Exp.(signed @@ of_word w)
  else Exp.(unsigned @@ of_word w)

let signed f = f true
let unsigned f = f false

let apply_signess signed e =
  if signed then Exp.signed e
  else Exp.unsigned e

let var signed width =
  Exp.tmp (int_of_bitwidth width) |>
  apply_signess signed

let reg find signed op = match op with
  | Op.Imm _ | Op.Fmm _ -> ppc_fail "reg operand expected"
  | Op.Reg x -> apply_signess signed (find x)

let const signed width value =
  let width = int_of_bitwidth width in
  let x = Word.of_int ~width value in
  apply_signess signed (Exp.of_word x)

let of_string signed s =
  let s = String.filter ~f:(fun c -> c <> '_') s in
  let chop (prefix, len) =
    match String.chop_prefix ~prefix s with
    | None -> None
    | Some data -> Some (len,data) in
  let width,data =
    match List.find_map ~f:chop ["0x",4; "0o",3; "0b",1;] with
    | Some (len, data) -> String.length data * len, data
    | None -> Z.numbits (Z.of_string s), s in
  let suf = if signed then "s" else "u" in
  let w = Word.of_string (sprintf "%s:%d%s" s width suf) in
  apply_signess signed (Exp.of_word w)

let zero = Exp.of_word Word.b0
let one  = Exp.of_word Word.b1
let ones w = Exp.of_word (Word.ones w)

let first e bits =
  let w = Exp.width e in
  Exp.extract (w - 1) (w - bits) e

let last e bits = Exp.extract (bits - 1) 0 e
let high w e = first e (int_of_bitwidth w)
let low w e = last e (int_of_bitwidth w)

let msb e =
  let h = Exp.width e - 1 in
  Exp.extract h h e

let lsb e = Exp.extract 0 0 e

let nth w e index =
  let width = Exp.width e in
  let step = int_of_bitwidth w in
  let x = width / step - index - 1 in
  let hi = (x + 1) * step - 1 in
  let lo = x * step in
  let n = width / step in
  let hi, lo =
    if n * step < width then
      let sh = width - n * step in
      hi + sh, lo + sh
    else hi, lo in
  Exp.extract hi lo e

let extract e left right =
  let width = Exp.width e in
  let target_width = right - left + 1 in
  if width >= target_width then
    let hi = width - left - 1 in
    let lo = width - right - 1 in
    Exp.extract hi lo e
  else
    Exp.extract (target_width - 1) 0 e

let when_ cond then_ = if_ cond then_ []
let ifnot cond else_ = if_ cond [] else_

type clause = [
  | `Case of (exp * rtl list)
  | `Default of rtl list
]

let case x y = `Case (x,y)
let default y = `Default y

let switch exp cases =
  let default = List.filter_map ~f:(function
      | `Default y -> Some y
      |  _ -> None) cases in
  let default = Option.value ~default:[] (List.hd default) in
  let cases = List.filter_map ~f:(function
      | `Case (x,y) -> Some (x,y)
      | _ -> None) cases in
  let cond x = Infix.(exp = x) in
  match cases with
  | [] -> ppc_fail "empty switch"
  | (x, code) :: [] -> (if_ (cond x) code default;)
  | (x, code) :: cases ->
    let else_ =
      List.fold (List.rev cases) ~init:default ~f:(fun acc (x,code) ->
          [if_ (cond x) code acc;]) in
    (if_ (cond x) code else_)

let width e =
  let w = Exp.width e in
  Exp.of_word (Word.of_int ~width:w w)
