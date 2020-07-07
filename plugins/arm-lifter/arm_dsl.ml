open Bap_core_theory
open Base
open KB.Syntax

module Env  = Arm_env.Env
module Common = Dsl_common

module Make_Extend(Core : Theory.Core)(Holder : Common.ValueHolder) = struct
  open Core

  module Arm_cpu = struct
    exception Assert_error
    include Env
    let assert_val (op : operand) = match op with
      | `Reg r -> load_reg r |> var
      | `Imm i -> (int value (Bap.Std.Word.to_bitvec i))
    let assert_var = function
      | `Reg r -> load_reg r
      | `Imm i -> raise Assert_error
  end
  module DSL = Dsl_common.DSL(Core)(Arm_cpu)(Holder)

  include DSL
end

module Make(Core : Theory.Core) = struct
  open Core

  module Arm_cpu = struct
    exception Assert_error
    include Env
    let assert_val (op : operand) = match op with
      | `Reg r -> load_reg r |> var
      | `Imm i -> (int value (Bap.Std.Word.to_bitvec i))
    let assert_var = function
      | `Reg r -> load_reg r
      | `Imm i -> raise Assert_error
  end
  module DSL = Dsl_common.DSL(Core)(Arm_cpu)(Arm_cpu)

  include DSL
end