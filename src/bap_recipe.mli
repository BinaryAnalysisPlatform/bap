open Core_kernel.Std

type error

type t

val load : ?paths:string list -> string -> (t,error) result

val argv : t -> string array

val cleanup : t -> unit

val descr : t -> string

val pp_error : Format.formatter -> error -> unit
