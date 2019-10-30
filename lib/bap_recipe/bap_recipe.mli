type error

type t
type param

val load :
  ?paths:string list ->
  ?env:(string * string) list ->
  string -> (t,error) result

(** [search paths] is a list of recipe names available in [paths].

    A file or a folder is considered to be a recipe if has the
    [.recipe] extension, therefore this function just returns the
    list of all files and directories that match this criterion. The
    names a chopped of the extension and dirnames.
*)
val search : string list -> string list

(** [argv recipe] returns the argument vector for recipe.*)
val argv : t -> string array
val close : t -> unit
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
