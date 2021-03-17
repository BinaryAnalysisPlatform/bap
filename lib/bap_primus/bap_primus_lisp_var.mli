open Core_kernel
open Bap_core_theory
open Bap_primus_lisp_types

type t = var [@@deriving compare, sexp_of]
include Comparable.S_plain with type t := t
val to_string : t -> string


type read_error = Empty | Not_a_var | Bad_type | Bad_format

val read : ?package:string -> Id.t -> Eq.t -> string -> (t,read_error) result
val reify : width:int -> t -> 'a Theory.Bitv.t Theory.Var.t
