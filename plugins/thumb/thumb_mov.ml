open Bap_core_theory
open Base
open KB.Syntax
open Thumb_core

module Make(CT : Theory.Core) = struct
  module T = Thumb_core.Make(CT)
  open T open T.Syntax

  let carry_from_add ~r ~rn  = bit (r < rn)

  let overflow_from_add ~r ~rn ~rm =
    msb @@ (rn lxor (lnot rm)) land (rn lxor r)

  let overflow_from_sub ~r ~rn ~rm =
    msb @@ (rn lxor rm) land (rn lxor r)

  let borrow_from_sub ~rn ~rm = bit (rn < rm)

  let movi8 rd x = data [
      rd := const x;
      nf := msb (var rd);
      cf := is_zero (var rd);
    ]

  let movsr rd rn = data [
      rd := var rn;
      nf := msb (var rd);
      cf := is_zero (var rd);
    ]

  let movr rd rn = data [
      rd := var rn
    ]

  let addi3 rd rn x = with_result rd @@ fun r -> [
      r := var rn + const x;
      nf := msb (var r);
      zf := is_zero (var r);
      cf := carry_from_add (var r) (var rn);
      vf := overflow_from_add (var r) (var rn) (const x);
    ]

  let addi8 rd x = with_result rd @@ fun r -> [
      r := var rd + const x;
      nf := msb (var r);
      zf := is_zero (var r);
      cf := carry_from_add (var r) (var rd);
      vf := overflow_from_add (var r) (var rd) (const x);
    ]

  let addrr rd rn rm = with_result rd @@ fun r -> [
      r := var rn + var rm;
      nf := msb (var r);
      zf := is_zero (var r);
      cf := carry_from_add (var r) (var rn);
      vf := overflow_from_add (var r) (var rn) (var rm);
    ]

  let addspi off = data [
      sp += const off;
    ]

  let addrspi rd off = data [
      rd := var sp + const off;
    ]

  let subi3 rd rn x = with_result rd @@ fun r -> [
      r := var rn - const x;
      nf := msb (var r);
      zf := is_zero (var r);
      cf := lnot @@ borrow_from_sub (var rn) (const x);
      vf := overflow_from_sub (var r) (var rn) (const x);
    ]

  let subi8 rd x = with_result rd @@ fun r -> [
      r := var rd - const x;
      nf := msb (var r);
      zf := is_zero (var r);
      cf := lnot @@ borrow_from_sub (var rd) (const x);
      vf := overflow_from_sub (var r) (var rd) (const x);
    ]

  let subrr rd rn rm = with_result rd @@ fun r -> [
      r := var rn - var rm;
      nf := msb (var r);
      zf := is_zero (var r);
      cf := lnot @@ borrow_from_sub (var rn) (var rm);
      vf := overflow_from_sub (var r) (var rn) (var rm);
    ]

  let subrspi rd off = data [
      rd := var sp - const off;
    ]

  let subspi off = data [
      sp -= const off;
    ]

end
