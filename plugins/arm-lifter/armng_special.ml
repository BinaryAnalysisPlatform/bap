open Bap_core_theory
open Base
open KB.Syntax
open Bap.Std

(* special instructions (including system insns.) *)
module Env  = Armng_env.Env
module Defs = Armng_defs
module Special(Core : Theory.Core) = struct
  open Core
  module DSL = Armng_dsl.Make(Core)
  module Flags = Armng_flags.Flags(Core)
  module Shift = Armng_shift.Shift(Core)
  module Cond = Armng_cond.Cond(Core)
  module Var = Theory.Var
  open Flags
  open Cond

  let cps2p : Theory.data Theory.eff list = []
  let dmb : Theory.data Theory.eff list = []
  let dsb : Theory.data Theory.eff list = []
  let hint : Theory.data Theory.eff list = []
  let pldi12 : Theory.data Theory.eff list = []

  let svc value cond = 
    let _exn_number =  Word.extract_exn ~hi:23 (Defs.assert_imm value) in
    DSL.[
      if_ (resolve_cond cond) [
        (* TODO : Cpu exception *)
      ]
    ]

  let mrs dest cond =
    let b0 = DSL.(imm 0 |> extend_to Env.bit_val) in
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest := concat Env.value [
            bool_as_bit (var Env.nf);
            bool_as_bit (var Env.zf);
            bool_as_bit (var Env.cf);
            bool_as_bit (var Env.vf);
            bool_as_bit (var Env.qf);
            b0; b0; b0; b0; b0; b0; b0; (* seven `0` for padding *)
            extract Env.bit_val !!3 !!3 (var Env.ge);
            extract Env.bit_val !!2 !!2 (var Env.ge);
            extract Env.bit_val !!1 !!1 (var Env.ge);
            extract Env.bit_val !!0 !!0 (var Env.ge);
          ] << imm 16
      ]
    ]

  let msr value src cond =
    let value = Defs.assert_imm value |> Word.to_int |> Or_error.ok_exn in
    DSL.[
      if_ (resolve_cond cond) [
        when_ Int.(value land 0x8 = 0x8) [
          Env.nf := nth_bit !!31 !$src;
          Env.zf := nth_bit !!30 !$src;
          Env.cf := nth_bit !!29 !$src;
          Env.vf := nth_bit !!28 !$src;
          Env.qf := nth_bit !!27 !$src;
        ];
        when_ Int.(value land 0x4 = 0x4) [
          Env.ge := concat Env.half_byte [
              extract Env.bit_val !!19 !!19 !$src;
              extract Env.bit_val !!18 !!18 !$src;
              extract Env.bit_val !!17 !!17 !$src;
              extract Env.bit_val !!16 !!16 !$src;
            ]
        ]
      ]
    ]


end