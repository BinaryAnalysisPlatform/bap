open Bap_core_theory
open Base
open KB.Syntax
open Bap.Std

module Env  = Arm_env.Env
module Defs = Arm_defs
module Mov(Core : Theory.Core) = struct
  open Core
  module DSL = Arm_dsl.Make(Core)
  module Flags = Arm_flags.Flags(Core)
  module Shift = Arm_shift.Shift(Core)
  module Cond = Arm_cond.Cond(Core)
  open Flags
  open Cond

  (* wflag is valid only when it's (`Reg `CPSR) *)
  let movi dest src cond wflag =
    DSL.[
      if_ (resolve_cond cond) [
        assert_var dest := assert_val src;
        when_ (is_cpsr wflag) [
          set_nzf (assert_var dest)
        ]
      ]
    ]

  (* this has exactly the same code as with `movi`, except the src variant is different *)
  let movr dest src cond wflag =
    DSL.[
      if_ (resolve_cond cond) [
        assert_var dest := assert_val src;
        when_ (is_cpsr wflag) [
          set_nzf (assert_var dest)
        ]
      ]
    ]

  let movsr dest src sreg simm cond wflag =
    let (shift_operand, carry) = Shift.shift_r src simm sreg in
    DSL.[
      if_ (resolve_cond cond) [
        assert_var dest := shift_operand;
        when_ (is_cpsr wflag) [
          set_nzf (assert_var dest);
          Env.cf := carry
        ]
      ]
    ]

  let movsi dest src simm cond wflag =
    let (shift_operand, carry) = Shift.shift_i src simm in
    DSL.[
      if_ (resolve_cond cond) [
        assert_var dest := shift_operand;
        when_ (is_cpsr wflag) [
          set_nzf (assert_var dest);
          Env.cf := carry
        ]
      ]
    ]

end