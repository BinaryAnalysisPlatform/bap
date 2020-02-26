open Bap.Std
open Bap_core_theory

type context
val context : context KB.obj KB.t
val arch : (context, arch) KB.slot

module Call : sig
  val create : string -> Stmt.t
  val dst : Stmt.t -> string option
end

module Core : Theory.Core
module Core_with_fp_emulation  : Theory.Core
