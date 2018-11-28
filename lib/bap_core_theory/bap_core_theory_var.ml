open Caml.Format
open Bap_knowledge
open Bap_core_theory_sort

type 'a t = {
  name : string;
  sort : 'a sort;
}


let create sort name = {sort; name}
let name v = v.name
let sort v = v.sort

module Generator : sig
  val fresh : 'a sort -> 'a t Knowledge.t
end  = struct
  let data = Semantics.declare "counter"
      (module Domain.Counter)
  let counter = Knowledge.declare
      ~name:"edu.cmu.ece.bap/fresh-variables"
      ~desc:"fresh variables generator"
      data

  open Knowledge.Syntax
  let succ = Domain.Counter.succ

  let fresh sort=
    Knowledge.collect counter Label.root >>= fun x ->
    Knowledge.provide counter Label.root (succ x) >>| fun () ->
    {sort; name = asprintf "$%a" Domain.Counter.pp (succ x)}
end
