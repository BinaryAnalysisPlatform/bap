open Bap_core_theory
open Base
open KB.Syntax

module Defs = Arm_defs
module Flags = Arm_flags.Flags

let package = "arm-lifter"

type insns = Defs.insn * (Defs.op list)

module ARM(Core : Theory.Core) = struct
  open Core
  open Defs
  module Env = Arm_env.Env
  module Utils = Arm_util.Utils(Core)
  module Mov = Arm_move.Mov(Core)
  module Bits = Arm_bits.Bits(Core)

  open Utils

  let move eff =
    KB.Object.create Theory.Program.cls >>= fun lbl ->
    blk lbl eff skip

  let ctrl eff data pc = 
    Theory.Label.for_addr pc >>= fun lbl ->
    blk lbl data eff

  let lift_move insn ops =
    match insn, ops with
    | _, _ -> pass


  let lift_bits insn ops =
    match insn, ops with
    | _, _ -> pass




  let lift_mult insn ops =
    match insn,ops with
    | _, _ -> pass


  let lift_mem insn ops =
    match insn, ops with
    | _, _ -> pass


  (** Branching instructions *)

  let lift_branch insn ops =
    match insn, ops with
    | _, _ -> skip


  let lift_special insn ops =
    match insn, ops with
    | _, _ -> pass

  let lift ((ins, ops) : insns) : unit Theory.Effect.t KB.t = 
    match ins with
    | #move_insn -> lift_move ins ops |> move
    | #mem_insn -> lift_mem ins ops |> move
    | #bits_insn -> lift_bits ins ops |> move
    | #mult_insn -> lift_mult ins ops |> move
    | #special_insn -> lift_special ins ops |> move
    (* this is malformed for now *)
    | #branch_insn -> ctrl (lift_branch ins ops) pass (Bitvec.zero) (* var Env.pc *)

end