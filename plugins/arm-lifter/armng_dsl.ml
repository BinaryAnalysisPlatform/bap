open Bap_core_theory
open Base
open KB.Syntax

module Env  = Armng_env.Env
module Env_fp = Armng_env_fp.Env_fp
module Common = Dsl_common

exception Assert_error
exception Null_Address_error

module Arm_cpu(Core : Theory.Core) = struct
  open Core
  include Env
  let assert_val (op : operand) = match op with
    | `Reg r -> load_reg r |> var
    | `Imm i -> (int value (Bap.Std.Word.to_bitvec i))
  let assert_var = function
    | `Reg r -> load_reg r
    | `Imm _ -> raise Assert_error
end

module Arm_fpu64(Core : Theory.Core) = struct
  open Core
  include Env_fp
  type value = f64_format
  let value = f64
  let assert_val (op : operand) = match op with
    | `Reg r -> load_dreg r |> fst |> var
    | `Imm i -> float f64 (int f64_bitv (Bap.Std.Word.to_bitvec i))
  let assert_var = function
    | `Reg r -> load_dreg r |> fst
    | `Imm _ -> raise Assert_error
end

module Make_FP64(Core : Theory.Core) = struct
  open Core
  module FPU = Arm_fpu64(Core)
  module DSL = Dsl_common.DSLFP(Core)(FPU)

  include DSL
end

module Make_Extend(Core : Theory.Core)(Holder : Common.ValueHolder) = struct
  open Core
  module CPU = Arm_cpu(Core)
  module DSL = Dsl_common.DSL(Core)(CPU)(Holder)

  include DSL

  let local_addr : Bitvec.t option ref = ref None

  let put_addr addr = Ref.(local_addr := Some addr)

  let get_addr = !local_addr

  (* renaming here, this yields a pure data effect rather than a mixed effect *)
  let (<==) = set

  let data eff = 
    let skip = perform Theory.Effect.Sort.bot in
    KB.Object.create Theory.Program.cls >>= fun lbl -> 
    blk lbl eff skip

  let ctrl addr eff =
    let pass = perform Theory.Effect.Sort.bot in
    Theory.Label.for_addr addr >>= fun lbl ->
    blk lbl pass eff

  let (!$) (op : Armng_defs.op) = match op with
    | `Reg `PC -> (match get_addr with
        | Some addr -> Core.int Env.value addr
        | _ -> raise Null_Address_error)
    | _ -> !$op

  let (:=) assignee var = 
    let pc_ident = Theory.Var.ident CPU.pc in
    let target_ident = Theory.Var.ident assignee in
    (* test if the assignee is identical to `PC` *)
    if Int.(Theory.Var.compare_ident pc_ident target_ident = 0) then
      match get_addr with
      | Some addr -> ctrl addr (jmp var)
      | _ -> raise Null_Address_error
    else data (set assignee var)

end

module Make(Core : Theory.Core) = struct
  module CPU_Holder = Arm_cpu(Core)
  module Nested = Make_Extend(Core)(CPU_Holder)

  include Nested

  let if_ cond eff = let bot = Core.perform Theory.Effect.Sort.bot in
    match cond with
    | `Var var -> Nested.if_ var eff
    | `Const const -> if const then eff |> Nested.expand else bot
end