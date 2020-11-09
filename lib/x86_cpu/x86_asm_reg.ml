open Core_kernel
open Bap.Std

include X86_asm_reg_types

let width = function
  | #r8 -> `r8
  | #r16 | `IP -> `r16
  | #r32 | `EIP -> `r32
  | #r64 | `RIP -> `r64
  | #r128 -> `r128
  | #r256 -> `r256


let bitwidth r = width r |> Size.in_bits

type spec = [`Nil | t] [@@deriving sexp]

let decode reg =
  match Reg.name reg |> Sexp.of_string |> spec_of_sexp with
  | `Nil -> None
  | #t as r -> Some r
  | exception _ ->
    failwithf "unknown register %s" (Reg.name reg) ()
