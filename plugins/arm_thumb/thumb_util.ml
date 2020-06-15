open Bap_core_theory
open Base
open KB.Syntax

module Env  = Thumb_env.Env

module Utils(Core : Theory.Core) = struct
open Core

    let skip : Theory.ctrl Theory.eff = perform Theory.Effect.Sort.bot
    let pass : Theory.data Theory.eff = perform Theory.Effect.Sort.bot

    let nop =
        KB.return @@
        Theory.Effect.empty Theory.Effect.Sort.top

    let reg = Env.load_reg

    let reg_wide = Env.load_reg_wide

    let bool_as_bitv b = ite b (int Env.value Bitvec.one) (int Env.value Bitvec.zero)

    let word_as_bitv w = (int Env.value (Bap.Std.Word.to_bitvec w))

    let bitv_of imm = word_as_bitv (Bap.Std.Word.of_int 32 imm)

    let move_reg ?lreg:(lreg = reg) dest src = set (lreg dest) (var (lreg src))

    let set_reg ?lreg:(lreg = reg) dest imm = 
        set (lreg dest) (word_as_bitv imm)

end