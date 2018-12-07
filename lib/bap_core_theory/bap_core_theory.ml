open Bap_knowledge

module Sort   = Bap_core_theory_sort.Sort
module Kind = Bap_core_theory_kind
module Bool = Bap_core_theory_sort.Bool
module Bits = Bap_core_theory_sort.Bits
module Mems = Bap_core_theory_sort.Mems
module Floats = Bap_core_theory_sort.Floats
module Rmode = Bap_core_theory_sort.Rmode
module Value = Bap_core_theory_value
module Eff = Bap_core_theory_eff
module Var = Bap_core_theory_var
module Link = Bap_core_theory_link



type 'a var = 'a Var.t
type 'a sort = 'a Sort.t
type 'a kind = 'a Kind.t
type 'a value = 'a Value.t
type 'a eff = 'a Eff.t
type 'a bitv = 'a Bits.t
type ('a,'b) mem = ('a,'b) Mems.t
type bit = Bool.t
type data = Kind.data = private Data_Effect
type ctrl = Kind.ctrl = private Ctrl_Effect
type rmode = Rmode.t
type 'f float = 'f Floats.t
type ('r,'s) format = ('r,'s) Floats.Format.t
type word = Bap.Std.Bitvector.t (* we will extract bitvector from BAP later *)

type 'a t = 'a knowledge

module Theory = struct
  type 'a t = 'a Knowledge.t
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
end

module IEEE754 = Bap_core_theory_IEEE754

module Grammar = Bap_core_theory_grammar_definition
module Parser = Bap_core_theory_parser
