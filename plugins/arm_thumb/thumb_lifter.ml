open Core_kernel
open Or_error
open Bap.Std

open Thumb_types
open Thumb_utils

module Basic     = Disasm_expert.Basic
module Env = Thumb_env

module Thumb = struct
  module CPU = struct 
  include Thumb_env

  let mem = mem
  let pc = pc
  let sp = sp


  let regs = Var.Set.of_list [
      r0; r1; r2; r3; r4;
      r5; r6; r7;
      pc;  sp; lr;
      spsr; cpsr; itstate;
    ]

  (* although PC is stricly speaking is GPR we will rule it out *)
  let non_gpr = Var.Set.of_list [
      pc; spsr; cpsr; itstate;
    ]

  let gpr = Var.Set.diff regs non_gpr

  let perms = Var.Set.of_list [
      r4; r5; r6; r7;
    ]

  let flags = Var.Set.of_list @@ [
      nf; zf; cf; qf; vf;
    ] @ Array.to_list ge

  let nf = nf
  let zf = zf
  let cf = cf
  let vf = vf

  let is = Var.same

  let is_reg r = Set.mem regs (Var.base r)
  let is_sp = is sp
  let is_bp _  = false
  let is_pc = is pc
  let addr_of_pc m = Addr.(Memory.min_addr m ++ 8)
  let is_flag r = Set.mem flags (Var.base r)
  let is_zf = is zf
  let is_cf = is cf
  let is_vf = is vf
  let is_nf = is nf

  let is_mem = is mem
  end
let resolve_pc mem = Stmt.map (object(self)
    inherit Stmt.mapper as super
    method! map_var var =
      if Var.(equal var CPU.pc) then
        Bil.int (CPU.addr_of_pc mem)
      else super#map_var var
  end)

let lift_move word ops (insn : move_insn) : stmt list = let open Thumb_mov in
 match insn, ops with
  | `tMOVr,  [|dest; src|] -> move dest src (* this inst. has no side effect *)
  | `tMOVi8, [|dest; src|] -> movei8 dest src
  | _ -> []
let lift_mem ops op = []
let lift_branch mem ops op = []

let insn_exn mem insn : bil Or_error.t =
  let name = Basic.Insn.name insn in
  Memory.(Addr.Int_err.(!$(max_addr mem) - !$(min_addr mem)))
  >>= Word.to_int >>= fun s -> Size.of_int ((s+1) * 8) >>= fun size ->
  Memory.get ~scale:(size ) mem >>| fun word ->
  match Thumb_insn.of_basic insn with
  | None -> [Bil.special (sprintf "unsupported: %s" name)]
  | Some arm_insn -> match Thumb_op.arm_ops (Basic.Insn.ops insn) with
    | Error err -> [Bil.special (Error.to_string_hum err)]
    | Ok ops -> match arm_insn with
      | #move_insn as op -> lift_move word ops op
      | #mem_insn  as op -> lift_mem  ops op
      | #branch_insn as op -> lift_branch mem ops op

let lift mem insn =
  try insn_exn mem insn >>| resolve_pc mem with
  | Lifting_failed msg -> errorf "%s:%s" (Basic.Insn.name insn) msg
  | exn -> of_exn ~backtrace:`Get exn
end