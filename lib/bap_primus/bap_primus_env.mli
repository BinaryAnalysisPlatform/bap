open Core_kernel
open Bap_core_theory
open Bap_primus_types

module Generator = Bap_primus_generator
module Machine = Bap_primus_machine

type exn += Undefined_var of Var.ident

val get : Var.ident -> value Machine.t
val set : Var.ident -> value -> unit Machine.t
val add : Var.ident -> Generator.t -> unit Machine.t
val has : Var.ident -> bool Machine.t
val all : Var.ident Sequence.t Machine.t
