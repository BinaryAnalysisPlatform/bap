open Bap_lisp__types
module Context = Bap_lisp__context
module Def = Bap_lisp__def
module Program = Bap_lisp__program
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
val primitive  : (Def.prim, unit, unit) one
val subst : (Def.subst, unit, unit) one
val const : (Def.const, unit, unit) one

val pp_resolution: Format.formatter -> resolution -> unit
