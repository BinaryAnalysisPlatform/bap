open Bap_knowledge

module KB = Knowledge

module Theory = struct
  module Value   = Bap_core_theory_value
  module Bool = Value.Bool
  module Bitv = Value.Bitv
  module Mem = Value.Mem
  module Float = Value.Float
  module Rmode = Value.Rmode
  module Effect = Bap_core_theory_effect
  module Var = Bap_core_theory_var
  module Program = Bap_core_theory_program
  module Label = Program.Label
  type program = Program.cls


  type 'a value = 'a Value.t
  type 'a effect = 'a Effect.t
  type 'a pure = 'a value knowledge
  type 'a eff = 'a effect knowledge


  type bool = Bool.t pure
  type 'a bitv = 'a Bitv.t pure
  type ('a,'b) mem = ('a,'b) Mem.t pure
  type 'f float = 'f Float.t pure
  type rmode = Rmode.t pure

  type data = Effect.Sort.data
  type ctrl = Effect.Sort.ctrl

  type ('r,'s) format = ('r,'s) Float.format

  type 'a var = 'a Var.t
  type word = Bitvec.t
  type label = program Knowledge.Object.t
  type cls = Bap_core_theory_definition.theory_cls
  let t = Bap_core_theory_definition.theory
  type theory = Bap_core_theory_definition.theory
  type t = theory

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

  type core = (module Core)
  module Basic = struct
    module Empty : Basic = Bap_core_theory_empty.Core
    module Make = Bap_core_theory_basic.Make
  end

  module Empty : Core = Bap_core_theory_empty.Core
  module IEEE754 = Bap_core_theory_IEEE754
  module Parser = Bap_core_theory_parser
  include Bap_core_theory_manager
end
