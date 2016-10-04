open Core_kernel.Std
open Bap.Std
open X86_opcode
open X86_tools_types

module type W = module type of Word
module Make (RR : RR) (FR : FR) (IV : IV) : PR = struct
  type t = [
    | `LOCK_PREFIX
    | `REP_PREFIX
    | `REPE_PREFIX
    | `REPNE_PREFIX
    | `EXCEPTION of int
    | `NONE
  ] [@@deriving sexp, compare]

  let parse mem op =
    let code = Memory.(mem ^! min_addr mem) |>
               Word.to_int |> ok_exn in
    match code, op with
    | 0xF0, #btx_mi
    | 0xF0, #btx_mr
    | 0xF0, #cmpxchg_rm -> `LOCK_PREFIX
    | 0xF0, _ -> `EXCEPTION (IV.to_int `UD)
    | 0xF3, #ins
    | 0xF3, #lods
    | 0xF3, #movs
    | 0xF3, #outs
    | 0xF3, #stos -> `REP_PREFIX
    | 0xF3, #cmps
    | 0xF3, #scas -> `REPE_PREFIX
    | 0xF2, #cmps
    | 0xF2, #scas -> `REPNE_PREFIX
    | _ -> `NONE

  let lock bil = Bil.special "lock" :: bil

  module Rep = struct
    let rcx =
      let reg = match RR.size with
        | `r32 -> `ECX
        | `r64 -> `RCX in
      RR.of_asm_exn reg |> RR.var

    let size = Size.in_bits RR.size
    let zf = FR.get `ZF

    let dec_rcx = Bil.(rcx := var rcx - int (Word.one size))
    let rcx_cond = Bil.(var rcx <> int (Word.zero size))
    let jmp mem = Bil.(jmp (int (Memory.min_addr mem)))

    let rep mem bil =
      let body = List.concat [bil; [dec_rcx; jmp mem]] in
      Bil.[ if_ rcx_cond body [] ]

    let repe mem bil =
      let zf = FR.get `ZF in
      let jmp = [jmp mem] in
      let body = List.concat [bil; [dec_rcx; Bil.if_ zf jmp []] ] in
      Bil.[ if_ rcx_cond body [] ]

    let repne mem bil =
      let zfne = FR.get `ZF |> Bil.lnot in
      let jmp = [jmp mem] in
      let body = List.concat [bil; [dec_rcx; Bil.if_ zfne jmp []] ] in
      Bil.[ if_ rcx_cond body [] ]
  end

  let rep = Rep.rep
  let repe = Rep.repe
  let repne = Rep.repne
end
