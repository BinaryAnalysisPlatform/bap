open Bap.Std

open Bap_primus_lisp_types
module Context = Bap_primus_lisp_context
module Def = Bap_primus_lisp_def
module Program = Bap_primus_lisp_program
module Value = Bap_primus_value

type resolution

type ('t,'a,'b) resolver =
  Program.t -> 't Program.item -> string -> 'a ->
  ('b,resolution) result option

type ('t,'a,'b) one = ('t,'a,'t Def.t * 'b) resolver
type ('t,'a,'b) many = ('t,'a,('t Def.t * 'b) list) resolver

val extern : (typ -> 'a -> bool) -> (Def.func, 'a list, (var * 'a) list) one
val defun  : (typ -> 'a -> bool) -> (Def.func, 'a list, (var * 'a) list) one
val meth : (typ -> 'a -> bool) -> (Def.meth, 'a list, (var * 'a) list) many
val macro : (Def.macro, tree list, (string * tree list) list) one
val primitive : (Def.prim, unit, unit) one
val semantics : ('a, unit, unit) one
val subst : (Def.subst, unit, unit) one
val const : (Def.const, unit, unit) one

val pp_resolution: Format.formatter -> resolution -> unit
