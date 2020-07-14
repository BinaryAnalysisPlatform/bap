open Bap_core_theory
open Base
open KB.Syntax

module Env  = Armng_env.Env
module Env_fp = Armng_env_fp.Env_fp
module Common = Dsl_common

exception Assert_error

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