open Bap_common
open Bap_bil

type t

val none : t
val read : t
val load : t
val store : t
val raise : t

val of_list : t list -> t

val reads : t -> bool
val loads : t -> bool
val stores : t -> bool
val raises : t -> bool

val has_effects : t -> bool
val has_coeffects :  t -> bool
val idempotent : t -> bool

val is_subset : t -> t -> bool

val compute : exp -> t
