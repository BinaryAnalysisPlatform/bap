open Core_kernel
open Regular.Std
open Bap.Std

type t [@@deriving bin_io, compare, sexp]
include module type of Uuidm with type t := t
include Regular.S with type t := t

val of_string: string -> t
