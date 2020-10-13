open Bap_core_theory
open Base
open KB.Syntax

open Thumb_core

type eff = unit Theory.effect KB.t

module Make(CT : Theory.Core) = struct
  module T = Thumb_core.Make(CT)
  open T
  open T.Syntax

  let holds cond =
    let is_set x = is_set ~@x and is_clear x = is_clear ~@x in
    match cond with
    | `EQ -> is_set zf
    | `NE -> is_clear zf
    | `CS -> is_set cf
    | `CC -> is_clear cf
    | `MI -> is_set nf
    | `PL -> is_clear nf
    | `VS -> is_set vf
    | `VC -> is_clear vf
    | `HI -> is_set cf && is_clear zf
    | `LS -> is_clear cf || is_set zf
    | `GE -> ~@nf = ~@vf
    | `LT -> ~@nf <> ~@vf
    | `GT -> is_clear zf && ~@nf = ~@vf
    | `LE -> is_set zf || ~@nf <> ~@vf
    | `AL -> assert false


  let b pc dst = goto (pc ++ dst)

  let bcc pc cnd dst = match cnd with
    | `AL -> b pc dst
    | cnd -> CT.branch (holds cnd)
               (goto (pc++dst))
               (seq [])

  let next pc = bitv W32.(pc + int 4)

  let bli pc dst = seq [
      data @@ [lr := next pc;];
      goto @@ pc++dst
    ]

  let mask = bitv@@W32.int32 0xffff_fffel

  let blr pc dst = seq CT.[
      data @@ [lr := next pc];
      ctrl @@ jmp (var dst land mask);
    ]
end
