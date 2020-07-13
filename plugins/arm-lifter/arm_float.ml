open Bap_core_theory
open Base
open KB.Syntax
open Bap.Std

module Env  = Arm_env.Env
module FP = Arm_env_fp.Env_fp
module Defs = Arm_defs


module Branch(Core : Theory.Core) = struct
  open Core
  module DSL = Arm_dsl.Make(Core)
  module FP64 = Arm_dsl.Make_FP64(Core)
  module Flags = Arm_flags.Flags(Core)
  module Shift = Arm_shift.Shift(Core)
  module Cond = Arm_cond.Cond(Core)
  module Var = Theory.Var
  open Flags
  open Cond
  
  (* fabsd Dd Dm cond *)
  let vabsd dest src cond =
    DSL.[
      if_ (resolve_cond cond) FP64.[
        !$$.dest := fabs !$.src
      ]
    ]

end