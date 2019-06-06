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
      ~equal:Bitvec.equal
      ~inspect:Bitvec_sexp.sexp_of_t

  let name = Knowledge.Domain.optional "name"
      ~equal:String.equal
      ~inspect:sexp_of_string

  let int = Knowledge.Domain.optional "ivec"
      ~equal:Int.equal
      ~inspect:sexp_of_int

  let attr name =
    let bool_t = Knowledge.Domain.optional ~equal:Bool.equal "bool" in
    Knowledge.Class.property ~package cls name bool_t


  let is_valid = attr "is-valid"
  let is_subroutine = attr "is-subroutine"


  let addr = Knowledge.Class.property ~package cls "label-addr" word
  let name = Knowledge.Class.property ~package cls "label-name" name
  let ivec = Knowledge.Class.property ~package cls "label-ivec" int

  open Knowledge.Syntax

  let for_name s =
    Knowledge.Symbol.intern ~package s cls >>= fun obj ->
    Knowledge.provide name obj (Some s) >>| fun () -> obj

  let for_addr x =
    let s = Bitvec.to_string x in
    Knowledge.Symbol.intern ~package s cls >>= fun obj ->
    Knowledge.provide addr obj (Some x) >>| fun () -> obj

  let for_ivec x =
    let s = sprintf "int-%d" x in
    Knowledge.Symbol.intern ~package:"label" s cls >>= fun obj ->
    Knowledge.provide ivec obj (Some x) >>| fun () -> obj

  include (val Knowledge.Object.derive cls)
end

module Semantics = struct
  type +'a cls = 'a Effect.spec
  let cls = Effect.bot
  include (val Knowledge.Value.derive Effect.top)
  let slot = Knowledge.Class.property program "semantics" domain
end

include (val Knowledge.Value.derive cls)
