type error

type t
type param



(** [load recipe] searches and loads a recipe.

    Searches a recipe in the search paths specified with the [paths]
    parameter, see the {!search} function for the description of the
    search rules.

    If a recipe is found then it is loaded. All recipes included in
    the found recipe are also loaded with the [load] function using
    the same [paths] and [env] arguments.

    During the loading recipe parameters are substituted using the
    [env] mapping.

    If a recipe includes files, they are unpacked into a separate
    folder, which will be removed when {!close} is called on the
    recipe.
*)
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


(** [args recipe] is an array of arguments specified in the recipe. *)
val args : t -> string array


(** [command recipe] returns the [recipe] command, if one exists.  *)
val command : t -> string option

(** [argv recipe] builds an argument vector from the [recipe].

    All arguments are appended to the passed [argv] (which defaults to
    an empty array, not [Sys.argv]). If [argv] contains [--] then
    arguments are prepended before [--] with everything after [--]
    left intact.

    If the [recipe] specifies a command then it is inserted after the
    first argument in [argv] (if such exists), unless the second
    argument already specifies the same command, in which case it is
    ignored.
*)
val argv : ?argv:string array -> t -> string array


(** [close recipe] closes the recipe and clears all associated resources.  *)
val close : t -> unit


(** [doc recipe] is the recipe description.  *)
val doc : t -> string


(** [params recipe] is the list of recipe parameters. *)
val params : t -> param list


(** [pp_error ppf err] prints the error message [err].   *)
val pp_error : Format.formatter -> error -> unit

(** Recipe Parameters.  *)
module Param : sig
  type t = param

  (** [name p] the parameter [p] name.  *)
  val name : param -> string

  (** [doc p] the parameter [p] description.  *)
  val doc : param -> string

  (** [default p] the default value of the parameter [p].  *)
  val default : param -> string

  (** [pp ppf p] prints the human-readable description of [p].  *)
  val pp : Format.formatter -> param -> unit
end
