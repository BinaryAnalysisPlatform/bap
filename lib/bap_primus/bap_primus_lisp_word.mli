open Core_kernel
open Bap_primus_lisp_types

type t = word [@@deriving compare, sexp_of]
type read_error = Empty | Not_an_int | Unclosed | Bad_literal | Bad_type

val read : Id.t -> Eq.t -> string -> (t,read_error) result
