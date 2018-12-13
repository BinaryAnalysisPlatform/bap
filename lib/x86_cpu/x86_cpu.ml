open Core_kernel.Std
open Bap.Std

open X86_env

module Make_CPU(Env : ModeVars) = struct
  include X86_env
  include Env
  (* we do not include pc into a set of gpr *)
  let gpr = Var.Set.of_list @@ [
      rax; rcx; rdx; rsi; rdi;
      rbx; rbp; rsp;
    ] @ Array.to_list r
      @ Array.to_list ymms

  let flags = Var.Set.of_list [
      cf; pf; af; zf; sf; oF; df
    ]

  let pc = rip
  let sp = rsp
  let bp = rbp
  let mem = mem
  let zf = zf
  let cf = cf
  let vf = oF
  let nf = sf

  let addr_of_pc = Memory.max_addr

  let is = Var.same
  let is_reg r = is pc r || Set.mem gpr (Var.base r)
  let is_flag r = Set.mem flags (Var.base r)
  let is_zf = is zf
  let is_cf = is cf
  let is_vf = is oF
  let is_nf = is sf
  let is_mem = is mem
  let is_sp = is sp
  let is_bp = is bp
  let is_pc = is pc
end



module AMD64 = Make_CPU(R64)
module IA32 = Make_CPU(R32)
