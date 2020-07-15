open Bap_core_theory
open Base
open KB.Syntax

module Env  = Thumb_env.Env
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

module Make_Extend(Core : Theory.Core)(Holder : Common.ValueHolder) = struct
  open Core
  module CPU = Arm_cpu(Core)
  module DSL = Dsl_common.DSL(Core)(CPU)(Holder)

  include DSL

  let assert_val_wide (op : Env.operand) = match op with
    | `Reg r -> Env.load_reg_wide r |> var
    | `Imm _ -> assert_val op

  let (!$+) = assert_val_wide

  let assert_var_wide (op : Env.operand) = match op with
    | `Reg r -> Env.load_reg_wide r
    | `Imm _ -> raise Assert_error

  let (!$$+) = assert_var_wide
end

module Make(Core : Theory.Core) = struct
  module CPU_Holder = Arm_cpu(Core)
  module Nested = Make_Extend(Core)(CPU_Holder)

  include Nested
end