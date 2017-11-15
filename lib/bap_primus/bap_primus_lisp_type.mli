open Core_kernel
open Bap_primus_lisp_types

type t = typ [@@deriving compare, sexp]
val read : string -> typ option

include Comparable.S_plain with type t := t
