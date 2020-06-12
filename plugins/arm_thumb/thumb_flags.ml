open Bap_core_theory
open Base
open KB.Syntax

module Env  = Thumb_env.Env

module Flags(Core : Theory.Core) = struct
open Core

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
end

(*
let set_vnzf_sub s1 s2 r t =
  Bil.move Env.vf (msb Bil.((s1 lxor s2) land (s1 lxor r))) ::
  set_nzf r t

let set_sub s1 s2 r t =
  Bil.move Env.cf Bil.(s2 <= s1) :: set_vnzf_sub s1 s2 r t

let set_adc s1 s2 r t =
  let sum_with_carry =
    let extend = Bil.(cast unsigned) (bitlen t + 1) in
    Bil.(extend s1 + extend s2 + extend (var Env.cf)) in
  Bil.move Env.cf (msb sum_with_carry) :: set_vnzf_add s1 s2 r t

let set_sbc s1 s2 r t = set_adc s1 Bil.(lnot s2) r t

let set_cf_data ~imm ~data =
  let value =
    let width = Word.bitwidth imm in
    if Word.(of_int ~width 255 >= imm && imm >= zero width) then
      let width = Word.bitwidth data in
      if Word.(Int_exn.(data land of_int ~width 0xf00) = zero width)
      then Bil.unknown "undefined" bool_t
      else Bil.int Word.b0
    else msb Bil.(int imm) in
  Bil.move Env.cf value*)