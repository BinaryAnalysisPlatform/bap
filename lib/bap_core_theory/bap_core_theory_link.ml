open Core_kernel
open Bitvec_order.Comparators
open Bap_knowledge



let package = "edu.cmu.ece.bap.core-theory"

type t = Link

let link = Knowledge.Class.declare ~package "link" Link
    ~desc:"a code reference"

let word = Knowledge.Domain.optional "word"
    ~inspect:Bitvec_sexp.sexp_of_t
    ~order:Bitvec_order.compare

let string = Knowledge.Domain.optional "string"
    ~inspect:sexp_of_string
    ~order:String.compare

let int = Knowledge.Domain.optional "string"
    ~inspect:sexp_of_int
    ~order:Int.compare


let addr = Knowledge.Class.property ~package link "link-addr" word
let name = Knowledge.Class.property ~package link "link-name" string
let ivec = Knowledge.Class.property ~package link "link-ivec" int
