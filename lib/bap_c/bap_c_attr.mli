open Bap.Std
open Bap_c_type

type 'a pass = attr -> 'a term -> 'a term

val register : sub pass -> unit
val apply : sub pass
