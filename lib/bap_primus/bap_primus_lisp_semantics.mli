open Bap_core_theory
open Bap.Std
open Bap_primus_lisp_types
open Bap_primus_lisp_program

type t = semantics
type value = unit Theory.Value.t
val create : insn -> value -> t
val effect : t -> insn
val result : t -> value

val reify : Theory.t -> program -> string -> t option KB.t
