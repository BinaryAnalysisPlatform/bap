open Bap_core_theory
open Base
open KB.Syntax

module Env  = Arm_env.Env

module ExtendValude = struct
  type value

  let value : value Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 33 (* 33 bits for carry flag inspection *)
end

module Flags(Core : Theory.Core) = struct
  open Core
  module DSL = Arm_dsl.Make(Core)
  module DSL_Extend = Arm_dsl.Make_Extend(Core)(ExtendValude)

  let set_nzf r : Theory.data Theory.eff =
    DSL.[
      Env.nf := (msb (var r));
      Env.zf := (var r = imm 0)
    ] |> DSL.expand

  let set_vnzf_add s1 s2 r = 
    DSL.[
      Env.vf := lnot (s1 lxor s2) land (s1 lxor var r) |> msb;
      set_nzf r
    ] |> DSL.expand

  let set_add s1 s2 r =
    DSL.[
      Env.cf := var r < s1;
      set_vnzf_add s1 s2 r
    ] |> DSL.expand

  let set_vnzf_sub s1 s2 r = 
    DSL.[
      Env.vf := s1 lxor s2 land s1 lxor var r |> msb;
      set_nzf r
    ] |> DSL.expand

  let set_sub s1 s2 r =
    DSL.[
      Env.cf := s1 <= s1;
      set_vnzf_sub s1 s2 r
    ] |> DSL.expand

  let as_bitv b = ite b 
      (int ExtendValude.value Bitvec.one) 
      (int ExtendValude.value Bitvec.zero)

  (* dsl arithmetic value is extended here for taking the msb *)
  let set_adc s1 s2 r =
    DSL_Extend.[
      Env.cf := s1 + s2 + (as_bitv (var Env.cf)) |> msb;
      set_vnzf_add s1 s2 r
    ] |> DSL.expand

  let set_sbc s1 s2 r = set_adc s1 (not s2) r

end