open Bap_core_theory
open Base
open KB.Syntax

module Env  = Thumb_env.Env
module Defs = Thumb_defs

exception Lift_Error = Thumb_defs.Lift_Error

module Bits(Core : Theory.Core) = struct
open Core

    module Utils = Thumb_util.Utils(Core)

    open Utils

    let sxtb dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        (set d (cast Env.byte b0 (var s) |> signed Env.value))
    | _ -> raise (Lift_Error "dest or src is not a register")

    let sxth dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        (set d (cast Env.half_word b0 (var s) |> signed Env.value))
    | _ -> raise (Lift_Error "dest or src is not a register")

    let uxtb dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        (set d (cast Env.byte b0 (var s) |> unsigned Env.value))
    | _ -> raise (Lift_Error "dest or src is not a register")

    let uxth dest src = match dest, src with
    | `Reg d, `Reg s -> let d, s = reg d, reg s in
        (set d (cast Env.half_word b0 (var s) |> unsigned Env.value))
    | _ -> raise (Lift_Error "dest or src is not a register")

end