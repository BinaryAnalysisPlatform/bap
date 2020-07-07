open Bap_core_theory
open Base
open KB.Syntax

module Env  = Arm_env.Env
module Common = Dsl_common

module Arm_cpu(Core : Theory.Core) = struct
  open Core
  exception Assert_error
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
end

module Make(Core : Theory.Core) = struct
  module CPU_Holder = Arm_cpu(Core)
  module Nested = Make_Extend(Core)(CPU_Holder)

  include Nested
end