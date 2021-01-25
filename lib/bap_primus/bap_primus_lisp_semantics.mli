open Bap_core_theory
open Bap.Std
open Bap_primus_lisp_types
open Bap_primus_lisp_program


type primitive

module Primitive : sig
  type t = primitive
  val name : t -> string
  val args : t -> unit Theory.Value.t list
end

(* val language : Theory.language
 * val program : (Theory.Source.cls, program) KB.slot
 * val primitive : (Theory.program, primitive option) KB.slot
 * val attribute : 'a Attribute.t -> (Theory.Label.t -> unit KB.t) -> unit *)


(*
   What about the attributes?

   Attributes map to properties of the program class, not really to
   its semantics property. When we create a program object that
   represents a lisp definition, we need to go through all attributes
   and store them in the created program.
   That means that for reach attribute that has a static
   interpretation, we will register an attribute reflection function that
   will take the attribute and the program object and initialize the
   corresponding fields.



 *)


(**

   Now, when we have primitives represented as promises, we have the
   following implementation

   {[

     module Prim = Primus.Lisp.Semantics.Primitive

     let () = KB.promise Theory.Semantics.slot @@ fun obj ->
       CT.instance () >>= fun (module Core) ->
       let open Prelude(Core) in
       KB.collect Primus.Lisp.Semantics.primitive obj >>=? fun p ->
       match Prim.name p, Prim.args p with
       | "+", args -> add args
       | ...
   ]}

   {[
     let () = Primus.Lisp.Theory.define "core:arithmetics" @@
       Theory.require theta >>= fun (module Core) ->
       let open Ops(Core) in [
         "+" ~> add;
         "-" ~> sub;
         "/" ~> div;
       ]

   ]}

*)

(**

   {[
     let () = Primus.Lisp.Theory.define "core:arithmetics" @@
       fun program ->
       let target = Primus.Lisp.target program in
       if Theory.Target.abi
     let open Ops(Core) in [
       "+" ~> add;
       "-" ~> sub;
       "/" ~> div;
     ]

   ]}

*)
