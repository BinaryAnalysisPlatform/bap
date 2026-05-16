open Bap_core_theory


val load : unit -> unit

type r8
type reg = r8 Theory.Bitv.t Theory.var

val parent : Theory.target

val tiny : Theory.target
val mega : Theory.target
val xmega : Theory.target

module Gcc : sig
  val args : reg list
  val rets : reg list
  val tiny : Theory.target
  val mega : Theory.target
  val xmega : Theory.target
end
