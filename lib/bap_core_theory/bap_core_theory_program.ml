open Core_kernel
open Bitvec_order.Comparators
open Bap_knowledge

let package = "core-theory"
module Effect = Bap_core_theory_effect

type cls = Program
type program = cls
let cls = Knowledge.Class.declare ~package "program" Program
let program = cls

module Label = struct
  let word = Knowledge.Domain.optional "word"
      ~inspect:Bitvec_sexp.sexp_of_t

  let string = Knowledge.Domain.optional "string"
      ~inspect:sexp_of_string

  let int = Knowledge.Domain.optional "string"
      ~inspect:sexp_of_int

  let addr = Knowledge.Class.property ~package cls "addr" word
  let name = Knowledge.Class.property ~package cls "name" string
  let ivec = Knowledge.Class.property ~package cls "ivec" int
  include (val Knowledge.Object.derive cls)
end

module Semantics = struct
  type +'a cls = 'a Effect.spec
  let cls = Effect.bot
  include (val Knowledge.Value.derive Effect.top)
  let slot = Knowledge.Class.property program "semantics" domain
end

include (val Knowledge.Value.derive cls)
