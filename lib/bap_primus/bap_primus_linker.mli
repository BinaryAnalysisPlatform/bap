open Regular.Std
open Bap_core_theory

open Bap_primus_types

module Machine = Bap_primus_machine
type name = Link.t
type exn += Unbound_name of name

val exec : name observation
val will_exec : name statement
val unresolved : name observation

val unresolved_handler : string

module Trace : sig
  val call : (string * value list) observation
  val call_entered : (string * value list) statement
  val return : (string * value list) observation
  val call_returned : (string * value list) statement
end

type 'a m = 'a Machine.t

val link : name -> unit m -> unit m

val lookup : name -> unit m option m
val unlink : name -> unit m
val exec : name -> unit m
val is_linked : name -> bool m
