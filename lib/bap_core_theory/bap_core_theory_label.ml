open Core_kernel
open Bitvec_order.Comparators
open Bap_knowledge

module Effect = Bap_core_theory_effect

let package = "core-theory"

let word = Knowledge.Domain.optional "word"
    ~inspect:Bitvec_sexp.sexp_of_t

let string = Knowledge.Domain.optional "string"
    ~inspect:sexp_of_string

let int = Knowledge.Domain.optional "string"
    ~inspect:sexp_of_int

let addr = Knowledge.Class.property ~package Effect.top "addr" word
let name = Knowledge.Class.property ~package Effect.top "name" string
let ivec = Knowledge.Class.property ~package Effect.top "ivec" int

include (val Knowledge.Object.derive Effect.top)
