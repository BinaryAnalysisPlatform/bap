open Bap_core_theory
open Base
open KB.Syntax
open Bap.Std

module Env  = Armng_env.Env
module Defs = Armng_defs

module ExtendValude = struct
  type value = Env.double_word

  let value : value Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 64 (* 33 bits for carry flag inspection *)
end

module Mul(Core : Theory.Core) = struct
  open Core
  module DSL = Armng_dsl.Make(Core)
  module DSL64 = Armng_dsl.Make_Extend(Core)(ExtendValude)
  module Flags = Armng_flags.Flags(Core)
  module Shift = Armng_shift.Shift(Core)
  module Cond = Armng_cond.Cond(Core)
  open Flags
  open Cond

  let mul dest src1 src2 cond _rflag wflag =
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest := !$src1 * !$src2;
        when_ (is_cpsr wflag) [
          set_nzf !$$dest
        ]
      ]
    ]

  let mla dest src1 src2 addend cond _rflag wflag =
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest := !$addend + !$src1 * !$src2;
        when_ (is_cpsr wflag) [
          set_nzf !$$dest
        ]
      ]
    ]

  let mls dest src1 src2 addend cond =
    DSL.[
      if_ (resolve_cond cond) [
        !$$dest := !$addend - !$src1 * !$src2;
      ]
    ]

  let umull lodest hidest src1 src2 cond _rflag wflag = 
    DSL.[
      if_ (resolve_cond cond) [
        Env.tmp64 := DSL64.(!$src1 * !$src2);
        !$$lodest := extract Env.value !!31 !!0 (var Env.tmp64);
        !$$hidest := extract Env.value !!61 !!32 (var Env.tmp64);
        when_ (is_cpsr wflag) [
          Env.nf := msb !$hidest;
          Env.zf := and_ (!$lodest = !!0) (!$hidest = !!0)
        ]
      ]
    ]

  let smull lodest hidest src1 src2 cond _rflag wflag = 
    DSL.[
      if_ (resolve_cond cond) [
        Env.tmp64 := DSL64.(!$src1 -* !$src2);
        !$$lodest := extract Env.value !!31 !!0 (var Env.tmp64);
        !$$hidest := extract Env.value !!61 !!32 (var Env.tmp64);
        when_ (is_cpsr wflag) [
          Env.nf := msb !$hidest;
          Env.zf := and_ (!$lodest = !!0) (!$hidest = !!0)
        ]
      ]
    ]

  let umlal lodest hidest src1 src2 cond _rflag wflag = 
    DSL.[
      if_ (resolve_cond cond) [
        Env.tmp64 := concat Env.double_word [!$hidest; !$lodest];
        Env.tmp64 := DSL64.(!$src1 * !$src2 + var Env.tmp64);
        !$$lodest := extract Env.value !!31 !!0 (var Env.tmp64);
        !$$hidest := extract Env.value !!61 !!32 (var Env.tmp64);
        when_ (is_cpsr wflag) [
          Env.nf := msb !$hidest;
          Env.zf := and_ (!$lodest = !!0) (!$hidest = !!0)
        ]
      ]
    ]

  let smlal lodest hidest src1 src2 cond _rflag wflag = 
    DSL.[
      if_ (resolve_cond cond) [
        Env.tmp64 := concat Env.double_word [!$hidest; !$lodest];
        Env.tmp64 := DSL64.(!$src1 -* !$src2 + var Env.tmp64);
        !$$lodest := extract Env.value !!31 !!0 (var Env.tmp64);
        !$$hidest := extract Env.value !!61 !!32 (var Env.tmp64);
        when_ (is_cpsr wflag) [
          Env.nf := msb !$hidest;
          Env.zf := and_ (!$lodest = !!0) (!$hidest = !!0)
        ]
      ]
    ]

  let smlabb dest src1 src2 accum cond _wflag =
    let halfword_extend src = DSL.(extend_to Env.half_word src |> extend_signed) in
    DSL.[
      if_ (resolve_cond cond) [
        Env.tmp64 := DSL64.(
            halfword_extend !$src1 -* 
            halfword_extend !$src2 + !$accum
          );
        !$$dest := extend (var Env.tmp64);
        (* overflow check *)
        Env.qf := extend_to Env.double_word !$dest <> var Env.tmp64;
      ]
    ]

  let smulbb dest src1 src2 cond _wflag =
    let halfword_extend src = DSL.(extend_to Env.half_word src |> extend_signed) in
    DSL.[
      if_ (resolve_cond cond) [
        Env.tmp64 := DSL64.(
            halfword_extend !$src1 -* 
            halfword_extend !$src2
          );
        !$$dest := extend (var Env.tmp64);
      ]
    ]

  let smlad dest src1 src2 accum cond _wflag =
    let halfword_extend src = DSL.(extend_to Env.half_word src |> extend_signed) in
    DSL.[
      if_ (resolve_cond cond) [
        Env.tmp64 := DSL64.(!$src1 -* !$src2);
        Env.tmp64 := DSL64.(
            halfword_extend !$src1 -* 
            halfword_extend !$src2 + !$accum
          );
        !$$dest := extend (var Env.tmp64);
        (* overflow check *)
        Env.qf := extend_to Env.double_word !$dest <> var Env.tmp64;
      ]
    ]

  let smuad dest src1 src2 cond _wflag =
    let halfword_extend src = DSL.(extend_to Env.half_word src |> extend_signed) in
    DSL.[
      if_ (resolve_cond cond) [
        Env.tmp64 := DSL64.(!$src1 -* !$src2);
        Env.tmp64 := DSL64.(
            halfword_extend !$src1 -* 
            halfword_extend !$src2
          );
        !$$dest := extend (var Env.tmp64);
        (* overflow check *)
        Env.qf := extend_to Env.double_word !$dest <> var Env.tmp64;
      ]
    ]

  let smlawb dest src1 src2 accum cond _wflag =
    let halfword_extend src = DSL.(extend_to Env.half_word src |> extend_signed) in
    DSL.[
      if_ (resolve_cond cond) [
        Env.tmp64 := DSL64.(!$src1 -* halfword_extend !$src2);
        !$$dest := extract Env.value !!47 !!16 (var Env.tmp64) + !$accum;
        (* overflow check *)
        Env.qf := extend_to Env.double_word !$dest <> var Env.tmp64;
      ]
    ]

  let smultb dest src1 src2 cond _wflag =
    let halfword_extend src = DSL.(extend_to Env.half_word src |> extend_signed) in
    DSL.[
      if_ (resolve_cond cond) [
        Env.tmp64 := DSL64.(!$src1 -* halfword_extend !$src2);
        !$$dest := extend (var Env.tmp64);
      ]
    ]

  let smlalbt dest hidest src1 src2 cond _wflag =
    let halfword_extend src = DSL.(extend_to Env.half_word src |> extend_signed) in
    DSL.[
      if_ (resolve_cond cond) [
        Env.tmp64 := DSL64.(halfword_extend !$src1 -* !$src2);
        !$$dest := extract Env.value !!31 !!0 (var Env.tmp64);
        !$$hidest := extract Env.value !!63 !!32 (var Env.tmp64);
      ]
    ]

end