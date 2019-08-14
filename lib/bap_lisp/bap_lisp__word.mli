open Core_kernel
open Bap_lisp__types

module Type = Bap_lisp__type

type t = word [@@deriving compare, sexp_of]
type read_error = Empty | Not_an_int | Unclosed | Bad_literal
                | Bad_type of Type.read_error

val read : Id.t -> Eq.t -> string -> (t,read_error) result
