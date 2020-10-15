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

  let sub r x y = [
    r := x - y;
    nf := msb (var r);
    zf := is_zero (var r);
    cf := lnot @@ borrow_from_sub x y;
    vf := overflow_from_sub (var r) x y;

  ]

  let subi3 rd rn x = with_result rd @@ fun r ->
    sub r (var rn) (const x)

  let subi8 rd x = with_result rd @@ fun r ->
    sub r (var rd) (const x)

  let subrr rd rn rm = with_result rd @@ fun r ->
    sub r (var rn) (var rm)

  let subrspi rd off = data [
      rd := var sp - const off;
    ]

  let subspi off = data [
      sp -= const off;
    ]

  let asri rd rm = function
    | 0 -> data [
        cf := msb (var rm);
        rd := CT.ite (CT.msb (var rm)) (const ~-1) (const 0);
        nf := msb (var rd);
        zf := is_zero (var rd);
      ]
    | n -> data [
        cf := nth Int.(n-1) (var rm);
        rd := var rm asr const n;
        nf := msb (var rd);
        zf := is_zero (var rd);
      ]

  let lsri rd rm = function
    | 0 -> data [
        cf := msb (var rm);
        rd := const 0;
        nf := bit0;
        zf := bit1;
      ]
    | n -> data [
        cf := nth Int.(n-1) (var rm);
        rd := var rm lsr const n;
        nf := msb (var rd);
        zf := is_zero (var rd);
      ]

  let lsli rd rm = function
    | 0 -> data [
        rd := var rm;
        nf := msb (var rd);
        zf := is_zero (var rd);
      ]
    | n -> data [
        cf := nth Int.(32-n) (var rm);
        rd := var rm lsl const n;
        nf := msb (var rd);
        zf := is_zero (var rd);
      ]

  let lorr rd rm = data [
      rd := var rd lor var rm;
      nf := msb (var rd);
      zf := is_zero (var rd);
    ]

  let cmpi8 rd x = Theory.Var.fresh s32 >>= fun r ->
    data @@ sub r (var rd) (const x)

  let cmpr rn rm = Theory.Var.fresh s32 >>= fun r ->
    data @@ sub r (var rn) (var rm)
end
