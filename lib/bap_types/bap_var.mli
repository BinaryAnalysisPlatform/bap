open Core_kernel.Std
open Regular.Std
open Bap_common
type t
include Regular.S with type t := t
val create : ?is_virtual:bool -> ?fresh:bool -> string -> typ -> t
val with_index : t -> int -> t
val index : t -> int
val base : t -> t
val same : t -> t -> bool
val name : t -> string
val typ : t -> typ
val is_virtual : t -> bool
val is_physical : t -> bool
module Id : Bap_state.S
