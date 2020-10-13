open Bap_core_theory
open Base
open KB.Syntax
open Thumb_core

module Mov(CT : Theory.Core) = struct
  module T = Thumb_core.Make(CT)
  open T open T.Syntax

  let carry ~r ~rd ~rr =
    let open CT in
    msb rd && msb rr ||
    msb rr && inv (msb r) ||
    msb r && inv (msb rd)

  let overflow ~r ~rd ~rr =
    let open CT in
    msb rd && msb rr && inv (msb r) ||
    inv (msb rd) && inv (msb rr) && (msb r)

  let overflow_from_sub ~r ~rn ~rm =
    msb @@ (rn lxor rm) land (rn lxor r)

  let borrow_from_sub ~rn ~rm = bit (rn < rm)

  (** [mov rd, #x]  *)
  let movi8 rd x = seq [
      rd := const x;
      nf := msb (var rd);
      cf := is_zero (var rd);
    ]

  (** [mov rd, rn]  *)
  let movsr rd rn = seq [
      rd := var rn;
      nf := msb (var rd);
      cf := is_zero (var rd);
    ]

  (** [mov rd, rn] with [d] or [n] greater than 7.  *)
  let tmovr rd rn = seq [
      rd := var rn
    ]

  (** [adds rd, rn, #x] aka add(1)  *)
  let addi3 rd rn x = with_result rd @@ fun r -> [

    ]

  (** [subs rd, rn, #x] aka sub(1) *)
  let subi3 rd rn x = with_result rd @@ fun r -> [
      r := var rn - const x;
      nf := msb (var r);
      zf := is_zero (var r);
      cf := lnot @@ borrow_from_sub (var rn) (const x);
      vf := overflow_from_sub (var r) (var rn) (const x);
    ]

  (** [subs rd, #x] aka sub(2)  *)
  let subi8 rd x = with_result rd @@ fun r -> [
      r := var rd - const x;
      nf := msb (var r);
      zf := is_zero (var r);
      cf := lnot @@ borrow_from_sub (var rd) (const x);
      vf := overflow_from_sub (var r) (var rd) (const x);
    ]

  (** [subs rd, rn, rm] aka sub(3) *)
  let subrr rd rn rm = with_result rd @@ fun r -> [
      r := var rn - var rm;
      nf := msb (var r);
      zf := is_zero (var r);
      cf := lnot @@ borrow_from_sub (var rn) (var rm);
      vf := overflow_from_sub (var r) (var rn) (var rm);
    ]

  (** [subs sp, #i] aka sub(4) *)
  let subspi off = seq [
      sp -= off;
    ]

end
