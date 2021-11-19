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

  let movi8 rd x = it_set rd (const x) @@ fun v -> [
      v := const x;
      nf := if Int.(x lsr 7 = 1) then bit1 else bit0;
      zf := if Int.(x = 0) then bit1 else bit0;
    ]

  let movsr rd rn = data [
      rd := var rn;
      nf := msb (var rd);
      zf := is_zero (var rd);
    ]

  let addi3 rd rn x = it_set rd (var rn + const x) @@ fun r -> [
      nf := msb (var r);
      zf := is_zero (var r);
      cf := carry_from_add (var r) (var rn);
      vf := overflow_from_add (var r) (var rn) (const x);
    ]

  let addi8 rd x = it_set rd (var rd + const x) @@ fun r -> [
      nf := msb (var r);
      zf := is_zero (var r);
      cf := carry_from_add (var r) (var rd);
      vf := overflow_from_add (var r) (var rd) (const x);
    ]

  let addrr rd rn rm = it_set rd (var rn + var rm) @@ fun r -> [
      nf := msb (var r);
      zf := is_zero (var r);
      cf := carry_from_add (var r) (var rn);
      vf := overflow_from_add (var r) (var rn) (var rm);
    ]

  let adcs rd rn rm =
    it_set rd (var rn + var rm + CT.unsigned s32 (var cf)) @@ fun r -> [
      nf := msb (var r);
      zf := is_zero (var r);
      cf := carry_from_add (var r) (var rn);
      vf := overflow_from_add (var r) (var rn) (var rm);
    ]

  let addspi off =
    it_set sp (var sp + const off) @@ fun _ -> []

  let addrspi rd off =
    it_set rd (var sp + const off) @@ fun _ -> []

  let cmp x y r = [
    nf := msb (var r);
    zf := is_zero (var r);
    cf := lnot @@ borrow_from_sub x y;
    vf := overflow_from_sub (var r) x y;
  ]


  let sub rd x y = it_set rd (x-y) (cmp x y)

  let subi3 rd rn x =
    sub rd (var rn) (const x)

  let subi8 rd x =
    sub rd (var rd) (const x)

  let subrr rd rn rm =
    sub rd (var rn) (var rm)

  let subrspi rd off =
    rd <-? var sp - const off

  let subspi off =
    sp <-? var sp - const off

  let asri rd rm = function
    | 0 ->
      it_set rd (CT.ite (CT.msb (var rm)) (const ~-1) (const 0))
      @@ fun rd -> [
        cf := msb (var rm);
        nf := msb (var rd);
        zf := is_zero (var rd);
      ]
    | n ->
      it_set rd (var rm asr const n) @@ fun rd -> [
        cf := nth Int.(n-1) (var rm);
        nf := msb (var rd);
        zf := is_zero (var rd);
      ]

  let lsri rd rm = function
    | 0 ->
      it_set rd (const 0) @@ fun _ -> [
        cf := msb (var rm);
        nf := bit0;
        zf := bit1;
      ]
    | n ->
      it_set rd (var rm lsr const n) @@ fun rd -> [
        cf := nth Int.(n-1) (var rm);
        nf := msb (var rd);
        zf := is_zero (var rd);
      ]

  let lsli rd rm = function
    | 0 -> it_set rd (var rm) @@ fun rd -> [
        nf := msb (var rd);
        zf := is_zero (var rd);
      ]
    | n -> it_set rd (var rm lsl const n) @@ fun rd -> [
        cf := nth Int.(32-n) (var rm);
        nf := msb (var rd);
        zf := is_zero (var rd);
      ]

  let lorr rd rm = it_set rd (var rd lor var rm) @@ fun rd -> [
      nf := msb (var rd);
      zf := is_zero (var rd);
    ]

  let cmpi8 rd x = Theory.Var.fresh s32 >>= fun r ->
    data @@ (r := var rd - const x) :: cmp (var rd) (const x) r

  let cmpr rn rm = Theory.Var.fresh s32 >>= fun r ->
    data @@ (r := var rn - var rm) :: cmp (var rn) (var rm) r
end
