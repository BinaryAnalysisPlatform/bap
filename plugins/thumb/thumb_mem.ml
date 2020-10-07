open Bap_core_theory
open Base
open KB.Syntax

module Env  = Thumb_env.Env
module Defs = Thumb_defs

module Mem(Core : Theory.Core) = struct
  open Core

  module Utils = Thumb_util.Utils(Core)

  open Utils

  (* The original ARM lifter implement this in a BIL loop-style for some reason *)
  let store_multiple dest src_list =
    match dest with
    | `Reg r -> let r = reg r in List.fold src_list ~init:pass
        ~f:(fun eff src -> match src with
            | `Reg s -> let src = reg s in seq eff
                (seq
                   (storew b0 (var Env.memory) (var r) (var src) |> set Env.memory)
                   (set r (add (var r) (bitv_of 4)))
                )
            | _ -> failwith "`src` must be a register"
          )
    | _ -> failwith "`dest` must be a register"

  let load_multiple dest src_list =
    match dest with
    | `Reg r ->
      if List.length src_list = 0 then pass
      else let first_eq = [%compare.equal: Defs.op] (List.nth_exn src_list 0) dest in
        let src_list = if first_eq
          then List.sub src_list ~pos:1 ~len:(List.length src_list - 1)
          else src_list in
        let r = reg r in
        Theory.Var.fresh Env.value >>= fun tmp ->
        seq (List.fold src_list ~init:(set tmp (var r))
               ~f:(fun eff src -> match src with
                   | `Reg s -> let src = reg s in seq eff
                       (seq
                          (loadw Env.value b0 (var Env.memory) (var tmp) |> set src)
                          (set tmp (add (var tmp) (bitv_of 4)))
                       )
                   | _ -> failwith "`src` must be a register"
                 )) (set r (var tmp))
    | _ -> failwith "`dest` must be a register"
  (* the `R` bit is automatically resolved *)
  let push_multiple src_list =
    Theory.Var.fresh Env.value >>= fun tmp ->
    let shift x = bitv_of 2 |> shiftl b0 x in
    let offset = List.length src_list |> bitv_of |> shift in
    let initial = set tmp (sub (var Env.sp) offset) in
    seq (List.fold src_list ~init:initial
           ~f:(fun eff src -> match src with
               | `Reg s -> let src = reg s in seq eff
                   (seq
                      (storew b0 (var Env.memory) (var tmp) (var src) |> set Env.memory)
                      (set tmp (add (var tmp) (bitv_of 4)))
                   )
               | _ -> failwith "`src` must be a register"
             )) (set Env.sp (sub (var Env.sp) offset))
  (* TODO: PC might change here *)
  let pop_multiple src_list pc =
    Theory.Var.fresh Env.value >>= fun tmp ->
    let initial = set tmp (var Env.sp) in
    seq (List.fold src_list ~init:initial
           ~f:(fun eff src -> match src with
               | `Reg `PC -> seq eff
                               (seq
                                  (logand
                                     (loadw Env.value b0 (var Env.memory) (var tmp))
                                     (bitv_of 0xfffffffe)
                                   |> set pc)
                                  (set tmp (add (var tmp) (bitv_of 4)))
                               )
               | `Reg s -> let src = reg s in seq eff
                   (seq
                      (loadw Env.value b0 (var Env.memory) (var tmp) |> set src)
                      (set tmp (add (var tmp) (bitv_of 4)))
                   )
               | _ -> failwith "`src` must be a register"
             )) (set Env.sp (var tmp))

  let lift_mem_single ?(sign = false) ?(shift_val = 2) dest src1 ?src2 (op : Defs.operation) (size : Defs.size) =
    let open Defs in
    let dest = match dest with
      | `Reg r -> reg r
      | _ -> failwith "`dest` must be a register"
    in
    let shift x = bitv_of 2 |> shiftl b0 x in
    let address = match src1, src2 with
      | `Reg s, None -> reg s |> var
      | `Reg s1, Some (`Reg s2) ->
        add (reg s1 |> var) (reg s2 |> var)
      | `Reg s, Some (`Imm v) ->
        add (reg_wide s |> var) (word_as_bitv v |> shift)
      | _ -> failwith "Unbound memory operation mode"
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
