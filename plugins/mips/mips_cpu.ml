open Core_kernel
open Bap.Std

open Mips_rtl
open Mips_dsl
open Mips_utils
open Mips_types
open Mips_model

let size_of_width x =
  let x = int_of_bitwidth x in
  match Size.of_int x with
  | Ok s -> s
  | Error _ -> mips_fail "invalid size: %d" x

let find m reg =
  let (module M : MIPS) = m in
  let open M in
  let open E in
  let find_reg regs reg = String.Map.find regs (Reg.name reg) in
  let find_gpr = find_reg gpr in
  let find_fpr = find_reg fpr in
  let reg_searches = [find_gpr; find_fpr] in
  List.filter_map reg_searches ~f:(fun f -> f reg) |> function
  | [] -> Exp.of_word (Word.zero gpr_bitwidth)
  | hd::[] -> hd
  | _ -> mips_fail "Register name %s is ambiguous!!!" (Reg.name reg)

let make_cpu addr_size endian memory =
  let (module M) = match addr_size with
    | `r32 -> (module MIPS_32 : MIPS)
    | `r64 -> (module MIPS_64) in
  let open M in
  let open E in
  let reg = reg (find (module M)) in
  let load addr width =
    let size = size_of_width width in
    Exp.load mem addr endian size in
  let store addr data width =
    let size = size_of_width width in
    store mem addr data endian size in
  let cia = Memory.min_addr memory |>
            Exp.of_word |>
            Exp.signed in
  let jmp e = match addr_size with
    | `r32 -> jmp (low word e)
    | `r64 -> jmp e in
  let find name regs n =
    try
      Int.Map.find_exn regs n
    with _ ->
      mips_fail "%s with number %d not found" name n in
  let gpr n = find "GPR" gpri n in
  let fpr n = find "FPR" fpri n in
  let word_width, word_bitwidth = match addr_size with
    | `r32 -> unsigned const byte 32, word
    | `r64 -> unsigned const byte 64, doubleword in
  { load; store; jmp; cia; word_width; word_bitwidth;
    reg; gpr; fpr; hi; lo;
  }
