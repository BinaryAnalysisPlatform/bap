open Bap_knowledge

module KB = Knowledge

module Theory = struct
  module Sort   = Bap_core_theory_sort.Sort
  module Bool = Bap_core_theory_sort.Bool
  module Bitv = Bap_core_theory_sort.Bitv
  module Mem = Bap_core_theory_sort.Mem
  module Float = Bap_core_theory_sort.Float
  module Rmode = Bap_core_theory_sort.Rmode
  module Effect = Bap_core_theory_effect
  module Var = Bap_core_theory_var
  module Label = Bap_core_theory_label

  type 'a sort = 'a Sort.t
  type 'a effect = 'a Effect.t

  type 'a t = 'a Knowledge.value Knowledge.t

  type 'a pure = 'a Sort.exp t
  type 'a eff = 'a Effect.spec t

  type bool = Bool.t pure
  type 'a bitv = 'a Bitv.t pure
  type ('a,'b) mem = ('a,'b) Mem.t pure
  type 'f float = 'f Float.t pure
  type rmode = Rmode.t pure

  type data = Effect.data
  type ctrl = Effect.ctrl

  type ('r,'s) format = ('r,'s) Float.format

  type 'a var = 'a Var.t
  type word = Bitvec.t
  type label = (unit Effect.spec) Knowledge.Object.t
  module type Init = Bap_core_theory_definition.Init
  module type Bool = Bap_core_theory_definition.Bool
  module type Bitv = Bap_core_theory_definition.Bitv
  module type Memory = Bap_core_theory_definition.Memory
  module type Effect = Bap_core_theory_definition.Effect
  module type Minimal = Bap_core_theory_definition.Minimal
  module type Basic = Bap_core_theory_definition.Basic
  module type Fbasic = Bap_core_theory_definition.Fbasic
  module type Float = Bap_core_theory_definition.Float
  module type Trans = Bap_core_theory_definition.Trans
  module type Core = Bap_core_theory_definition.Core

  module Basic = struct
    module Empty : Basic = Bap_core_theory_empty.Core
    module Make = Bap_core_theory_basic.Make
  end

  module Core = struct
    module Empty : Core = Bap_core_theory_empty.Core
  end

  module Manager = Bap_core_theory_manager.Theory
  let register = Bap_core_theory_manager.register

  module IEEE754 = Bap_core_theory_IEEE754

  module Grammar = Bap_core_theory_grammar_definition
  module Parser = Bap_core_theory_parser

end
