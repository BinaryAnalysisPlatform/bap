type error

type t
type param

val load :
  ?paths:string list ->
  ?env:(string * string) list ->
  string -> (t,error) result

val argv : t -> string array
val cleanup : t -> unit
val doc : t -> string
val params : t -> param list
val pp_error : Format.formatter -> error -> unit


module Param : sig
  type t = param
  val name : param -> string
  val doc : param -> string
  val default : param -> string
  val pp : Format.formatter -> param -> unit
end
