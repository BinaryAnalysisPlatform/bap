open Bap_core_theory
open Base
open KB.Syntax
open Bap.Std

module Env  = Armng_env.Env
module Defs = Armng_defs
module Bits(Core : Theory.Core) = struct
  open Core
  module DSL = Armng_dsl.Make(Core)
  module Flags = Armng_flags.Flags(Core)
  module Shift = Armng_shift.Shift(Core)
  module Cond = Armng_cond.Cond(Core)
  module Var = Theory.Var
  open Flags
  open Cond

  let rotate_right src amount =
    Shift.shift_with_carry src `ROR amount |> fst

  let arshift_right src amount =
    Shift.shift_with_carry src `ASR amount |> fst

  let (>>>) = rotate_right

  let (|>>) = arshift_right

  let uxtb dest src rot cond = 
    DSL.[
      if_ (resolve_cond cond)[
        !$$dest := (!$src >>> !$rot * imm 8) land imm 0x000000ff
      ]
    ]

  let uxth dest src rot cond = 
    DSL.[
      if_ (resolve_cond cond)[
        !$$dest := (!$src >>> !$rot * imm 8) land imm 0x0000ffff
      ]
    ]

  let sxtb dest src rot cond =
    DSL.[
      if_ (resolve_cond cond)[
        !$$dest := extend_to Env.byte (!$src >>> !$rot * imm 8) |> extend_signed
      ]
    ]

  let sxth dest src rot cond =
    DSL.[
      if_ (resolve_cond cond)[
        !$$dest := extend_to Env.half_word (!$src >>> !$rot * imm 8) |> extend_signed
      ]
    ]

  let uxtab dest src1 src2 rot cond =
    DSL.[
      local_var >>= fun tmp ->
      if_ (resolve_cond cond)[
        tmp := extend_to Env.byte (!$src2 >>> !$rot * imm 8) |> extend;
        !$$dest := !$src1 + var tmp
      ]
    ]

  let uxtah dest src1 src2 rot cond =
    DSL.[
      local_var >>= fun tmp ->
      if_ (resolve_cond cond)[
        tmp := extend_to Env.half_word (!$src2 >>> !$rot * imm 8) |> extend;
        !$$dest := !$src1 + var tmp
      ]
    ]

  let sxtab dest src1 src2 rot cond =
    DSL.[
      local_var >>= fun tmp ->
      if_ (resolve_cond cond)[
        tmp := extend_to Env.byte (!$src2 >>> !$rot * imm 8) |> extend_signed;
        !$$dest := !$src1 + var tmp
      ]
    ]

  let sxtah dest src1 src2 rot cond =
    DSL.[
      local_var >>= fun tmp ->
      if_ (resolve_cond cond)[
        tmp := extend_to Env.half_word (!$src2 >>> !$rot * imm 8) |> extend_signed;
        !$$dest := !$src1 + var tmp
      ]
    ]

  let ubfx dest src lsb widthm1 cond =
    DSL.[
      if_ (resolve_cond cond)[
        !$$dest := extract Env.value (!$lsb + !$widthm1) !$lsb !$src
      ]
    ]

  let sbfx dest src lsb widthm1 cond =
    let width = Word.to_int (Defs.assert_imm widthm1) |> Or_error.ok_exn in
    let value_type = Theory.Bitv.define width in
    DSL.[
      if_ (resolve_cond cond)[
        !$$dest := extract value_type (!$lsb + !$widthm1) !$lsb !$src |> signed Env.value
      ]
    ]

  (** llvm-mc has kind of wierd definition for this *)
  let bfi dest src lsb width cond =
    DSL.[
      local_var >>= fun tmp ->
      if_ (resolve_cond cond) [
        tmp := extract Env.value (!$lsb + !$width) !$lsb !$dest << !$lsb;
        !$$dest := (!$dest lxor var tmp) land 
                   (extract Env.value !$width (imm 0) !$src << !$lsb)
      ]
    ]

  let bfc dest lsb width cond =
    DSL.[
      local_var >>= fun tmp ->
      if_ (resolve_cond cond) [
        tmp := extract Env.value (!$lsb + !$width) !$lsb !$dest << !$lsb;
        (* clear bit field *)
        !$$dest := !$dest lxor var tmp
      ]
    ]

  let rbit dest src cond =
    DSL.[
      local_var >>= fun tmp ->
      if_ (resolve_cond cond) [
        tmp := imm 0;
        while_ (fun i -> Int.(i <= 31, i + 1)) 0 (fun i -> !% [
            tmp := var tmp lor bool_as_bitv (msb (!$src << imm i));
            tmp := var tmp << imm 1;
          ]) |> expand;
        !$$dest := var tmp;
      ]
    ]

  let swpb dest src1 src2 cond =
    DSL.[
      local_var >>= fun tmp ->
      if_ (resolve_cond cond) [
        tmp := load (var Env.memory) !$src2 |> extend;
        Env.memory := store (var Env.memory) !$src2 (extend_to Env.byte !$src1);
        !$$dest := var tmp;
      ]
    ]

  let pkhtb dest src1 src2 shift cond =
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest := concat Env.value [
            extract Env.half_word !!31 !!16 !$src1;
            extract Env.half_word !!15 !!0 (!$src2 |>> !$shift)
          ]
      ]
    ]

  let rev dest src cond =
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest := concat Env.value [
            extract Env.byte !!7 !!0 !$src;
            extract Env.byte !!15 !!8 !$src;
            extract Env.byte !!23 !!16 !$src;
            extract Env.byte !!31 !!24 !$src;
          ]
      ]
    ]

  let rev16 dest src cond =
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest := concat Env.value [
            extract Env.byte !!23 !!16 !$src;
            extract Env.byte !!31 !!24 !$src;
            extract Env.byte !!7 !!0 !$src;
            extract Env.byte !!15 !!8 !$src;
          ]
      ]
    ]

  let clz dest src cond =
    DSL.[
      local_var >>= fun tmp ->
      if_ (resolve_cond cond) [
        tmp := !$src;
        !$$dest := !!0;
        (* this couldn't by statically expanded *)
        repeat (msb (var tmp)) !%[
          tmp := var tmp << !!1;
          !$$dest := !$dest + imm 1
        ]
      ]
    ]


end