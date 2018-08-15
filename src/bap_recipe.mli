open Core_kernel.Std

type error

type t
type param

val load : ?paths:string list -> string -> (t,error) result
val argv : t -> string array
val cleanup : t -> unit
val descr : t -> string
val params : t -> param list
val pp_error : Format.formatter -> error -> unit
val pp_param : Format.formatter -> param -> unit
