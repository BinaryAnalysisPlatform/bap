open Bap_core_theory
open Base
open KB.Syntax
open Thumb_core

module Make(CT : Theory.Core) = struct
  open Thumb_core.Make(CT)
  open Syntax

  let sx rd rm =
    rd <-? CT.signed s32 (var rm)

  let ux rd rm =
    rd <-? CT.unsigned s32 (var rm)
end
