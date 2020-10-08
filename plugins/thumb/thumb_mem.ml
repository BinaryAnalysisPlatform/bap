open Bap_core_theory
open Base
open KB.Syntax

module Env  = Thumb_env.Env
type reg = Env.value Theory.Bitv.t Theory.var
type eff = unit Theory.effect KB.t
module M64 = Bitvec.M64


module Make(CT : Theory.Core) = struct
  let rec seq = function
    | [] -> CT.perform Theory.Effect.Sort.bot
    | [x] -> x
    | x :: xs -> CT.seq x @@ seq xs

  let foreach xs f = seq @@ List.concat_map xs ~f
  let foreachi xs f = seq @@ List.concat_mapi xs ~f
  let unsigned x = CT.unsigned Env.value x
  let signed x = CT.signed Env.value x
  let half x = CT.low Env.half_word x
  let byte x = CT.low Env.byte x

  let bitv x = CT.int Env.value x
  let const x = bitv (M64.int x)
  let int64 x = bitv (M64.int64 x)

  let var = CT.var
  let (:=) = CT.set
  let (+) = CT.add
  let (+=) r x = r := var r + x

  let loadb p = CT.load (var Env.memory) p
  let loadh p = CT.loadw Env.half_word CT.b0 (var Env.memory) p
  let loadw p = CT.loadw Env.value CT.b0 (var Env.memory) p

  let storeb p x = Env.memory := CT.store (var Env.memory) p x
  let storew p x = Env.memory := CT.storew CT.b0 (var Env.memory) p x

  let (<--) = storew

  let data eff =
    KB.Object.create Theory.Program.cls >>= fun lbl ->
    CT.blk lbl (seq eff) (seq [])

  (**************************************************************)

  let ldri rd r i = data [
      rd := loadw (var r + int64 i)
    ]

  let ldrr rd rn rm = data [
      rd := loadw (var rn + var rm);
    ]

  let ldrbi rd rn i = data [
      rd := unsigned @@ loadb (var rn + int64 i)
    ]

  let ldrbr rd rn rm = data [
      rd := unsigned @@ loadb (var rn + var rm)
    ]

  let ldrsb rd rn rm = data [
      rd := signed @@ loadb (var rn + var rm)
    ]

  let ldrhi rd rn i = data [
      rd := unsigned @@ loadh (var rn + int64 i)
    ]

  let ldrhr rd rn rm = data [
      rd := unsigned @@ loadh (var rn + var rm)
    ]

  let ldrsh rd rn rm = data [
      rd := signed @@ loadh (var rn + var rm);
    ]

  let ldrpci rd pc off = data [
      rd := loadw @@ bitv pc + int64 off;
    ]

  let ldrspi rd i = data [
      rd := loadw @@ var Env.sp + int64 i
    ]

  let ldmu b regs = data [
      foreach regs @@ fun r -> [
        r := loadw @@ var b;
        b += const 4;
      ]
    ]

  let ldm b regs = data [
      foreachi regs @@ fun i r -> [
        r := loadw @@ (var b + const i)
      ]
    ]

  let stri rd rm i = data [
      var rd <-- var rm + int64 i
    ]

  let strr rd rm rn = data [
      var rd <-- var rm + var rn
    ]

  let strhi rd rm i = data [
      var rd <-- half (var rm + int64 i);
    ]

  let strhr rd rm rn = data [
      var rd <-- half (var rm + var rn);
    ]

  let strbi rd rm i = data [
      var rd <-- byte (var rm + int64 i);
    ]

  let strbr rd rm rn = data [
      var rd <-- byte (var rm + var rn)
    ]

  let strsp rd i = data [
      var rd <-- var Env.sp + int64 i;
    ]

  let stm i regs = data [
      foreach regs @@ fun r -> [
        var i <-- var r;
        i += const 4;
      ]
    ]
end
