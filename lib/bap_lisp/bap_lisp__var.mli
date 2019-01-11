open Core_kernel
open Bap_lisp__types

type t = var [@@deriving compare, sexp_of]
include Comparable.S_plain with type t := t
val to_string : t -> string


type read_error = Empty | Not_a_var | Bad_type | Bad_format

val read : Id.t -> Eq.t -> string -> (t,read_error) result
