open Bap_core_theory
open Base
open KB.Syntax

module Env  = Thumb_env.Env

module Flags(Core : Theory.Core) = struct
  open Core
  type value_extend

  let value : value_extend Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 33 (* 33 bits for carry flag inspection *)

  let set_nzf r = 
    seq 
      (set (Env.nf) (msb (var r))) 
      (set (Env.zf) (eq (var r) (int Env.value (Bitvec.zero))))

  let set_vnzf_add s1 s2 r = 
    seq
      (set (Env.vf) (msb (logand (not (logxor s1 s2)) (logxor s1 (var r)))))
      (set_nzf r)

  let set_add s1 s2 r =
    seq
      (set Env.cf (slt (var r) s1))
      (set_vnzf_add s1 s2 r)

  let set_vnzf_sub s1 s2 r = 
    seq
      (set (Env.vf) (msb (logand (logxor s1 s2) (logxor s1 (var r)))))
      (set_nzf r)

  let set_sub s1 s2 r =
    seq
      (set Env.cf (sle s2 s1))
      (set_vnzf_sub s1 s2 r)

  let as_bitv b = ite b (int value Bitvec.one) (int value Bitvec.zero)

  let set_adc s1 s2 r =
    let sum_with_carry =
      let extend = cast value b0 in
      add (add (extend s1) (extend s2)) (as_bitv (var Env.cf))
    in seq 
      (set Env.cf (msb sum_with_carry))
      (set_vnzf_add s1 s2 r)

  let set_sbc s1 s2 r = set_adc s1 (not s2) r

end
