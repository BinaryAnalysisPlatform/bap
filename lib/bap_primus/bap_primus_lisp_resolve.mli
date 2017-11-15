open Bap.Std

open Bap_primus_lisp_types
module Context = Bap_primus_lisp_context
module Def = Bap_primus_lisp_def


type resolution



type ('t,'a,'b) resolver =
  't Def.t list -> Context.t -> string -> 'a ->
  resolution * ('t Def.t * 'b) option

val extern : arch -> (Def.func, value list, (var * value) list) resolver
val defun : arch -> (Def.func, value list, (var * value) list) resolver
val macro : (Def.macro, sexp list, (string * sexp list) list) resolver
val primitive : ('a Def.primitive, unit, unit) resolver
