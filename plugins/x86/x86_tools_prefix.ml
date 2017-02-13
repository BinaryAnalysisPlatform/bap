open Core_kernel.Std
open Bap.Std
open X86_tools_types

module Make (RR : RR) (FR : FR) (IV : IV) : PR = struct
  type t = X86_prefix.t [@@deriving sexp, compare]

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
