open Core_kernel
open Bap.Std
open Regular.Std

type t

val mark_updated : 'a term -> 'a term

val create : deads:Tid.Set.t -> sub term -> t

val apply : sub term -> t -> sub term


val update : sub term -> t -> sub term

val find_unreachable : sub term -> t -> t

val remove_dead_code : sub term -> t -> sub term


include Data.S with type t := t
