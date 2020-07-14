open Bap_core_theory
open Base
open KB.Syntax
open Bap.Std

module Env  = Armng_env.Env
module FP = Armng_env_fp.Env_fp
module Defs = Armng_defs


module Branch(Core : Theory.Core) = struct
  open Core
  module DSL = Armng_dsl.Make(Core)
  module FP64 = Armng_dsl.Make_FP64(Core)
  module Flags = Armng_flags.Flags(Core)
  module Shift = Armng_shift.Shift(Core)
  module Cond = Armng_cond.Cond(Core)
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