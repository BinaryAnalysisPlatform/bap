open Bap_core_theory
open Base
open KB.Syntax

module Env  = Thumb_env.Env
module Defs = Thumb_defs

module Bits(Core : Theory.Core) = struct
  open Core

  module Utils = Thumb_util.Utils(Core)
  module DSL = Thumb_dsl.Make(Core)

  open Utils

  let sxtb dest src =
    DSL.[
      !$$dest := extend_to Env.byte !$src |> extend_signed
    ]

  let sxth dest src =
    DSL.[
      !$$dest := extend_to Env.half_word !$src |> extend_signed
    ]

  let uxtb dest src =
    DSL.[
      !$$dest := extend_to Env.byte !$src |> extend
    ]

  let uxth dest src =
    DSL.[
      !$$dest := extend_to Env.half_word !$src |> extend
    ]

end
