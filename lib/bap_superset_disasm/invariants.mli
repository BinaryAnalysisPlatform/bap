open Bap.Std

module Dis = Disasm_expert.Basic

val tag_success : Superset_impl.t -> mem -> Dis.full_insn option -> Brancher.dests -> Superset_impl.t

val tag : ?invariants:((Superset_impl.t -> mem -> Dis.full_insn option -> Brancher.dests -> Superset_impl.t) list) ->
        Bap.Std.mem * Superset.Dis.full_insn option ->
            Superset_impl.t -> Superset_impl.t

val default_tags : (string * (Superset_impl.t -> mem -> Dis.full_insn option -> Brancher.dests -> Superset_impl.t)) list

