open Bap_core_theory
open Base
open KB.Syntax

module Env  = Thumb_env.Env
module Defs = Thumb_defs

exception Lift_Error = Thumb_defs.Lift_Error

module Mem(Core : Theory.Core) = struct
open Core

    module Utils = Thumb_util.Utils(Core)

    open Utils

    let lift_mem_single ?(sign = false) dest src1 ?src2 (op : Defs.operation) (size : Defs.size) =
        let open Defs in
        let dest = match dest with
            | `Reg r -> reg r
            | _ -> raise (Lift_Error "`dest` must be a register")
        in
        let shift x = bitv_of 2 |> shiftl b0 x in
        let address = match src1, src2 with
            | `Reg s, None -> reg s |> var
            | `Reg s1, Some (`Reg s2) -> 
                add (reg s1 |> var) (reg s2 |> var)
            | `Reg s, Some (`Imm v) ->
                add (reg_wide s |> var) (word_as_bitv v |> shift)
            | _ -> raise (Lift_Error "Unbound memory operation mode")
        in match op with
            | Ld -> 
            let extend = if sign then signed else unsigned in
            let value = match size with
                | W -> loadw Env.value b0 (var Env.memory) address
                | H -> loadw Env.half_word b0 (var Env.memory) address |> extend Env.value
                | B -> load (var Env.memory) address |> extend Env.value
            in set dest value
            | St -> 
            let mem = match size with
                | W -> storew b0 (var Env.memory) address (var dest)
                | H -> storew b0 (var Env.memory) address (cast Env.half_word b0 (var dest))
                | B -> store (var Env.memory) address (cast Env.byte b0 (var dest))
            in set Env.memory mem

end