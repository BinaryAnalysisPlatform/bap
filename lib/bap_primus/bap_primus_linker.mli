open Regular.Std
open Bap_core_theory
open Bap_knowledge

open Bap_primus_types

module Machine = Bap_primus_machine
type exn += Unbound_name of Label.t

val exec : Label.t observation
val will_exec : Label.t statement
val unresolved : Label.t observation

val unresolved_handler : Label.t Machine.t

module Trace : sig
  val call : (string * value list) observation
  val call_entered : (string * value list) statement
  val return : (string * value list) observation
  val call_returned : (string * value list) statement
end

val link : Label.t -> unit Machine.t -> unit Machine.t
val lookup : Label.t -> unit Machine.t option Machine.t
val unlink : Label.t -> unit Machine.t
val exec : Label.t -> unit Machine.t
val is_linked : Label.t -> bool Machine.t
