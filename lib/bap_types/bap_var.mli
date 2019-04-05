open Core_kernel
open Bap_core_theory

open Regular.Std
open Bap_common
type t
include Regular.S with type t := t

val reify : 'a Theory.var -> t
val ident : t -> Theory.Var.ident
val sort  : t -> Theory.Sort.top

(* Old interface  *)
val create : ?is_virtual:bool -> ?fresh:bool -> string -> typ -> t
val with_index : t -> int -> t
val index : t -> int
val base : t -> t
val same : t -> t -> bool
val name : t -> string
val typ : t -> typ
val is_virtual : t -> bool
val is_physical : t -> bool
