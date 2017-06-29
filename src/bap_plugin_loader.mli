open Bap.Std

exception Plugin_not_found of string

(** [run env argv] extracts and loads plugins from a command line
    arguments, specified by [argv] parameter.  Returns an array of
    command line arguments with plugin specific options removed.
    Load plugins only for provided environment [env]. *)
val run : string list -> string array -> string array

(** [run_and_get_passes env argv] loads plugins specified in the command line arguments
    passed as [argv] and returns [argv,passes], where [argv] is an
    array of command line arguments with all plugin specific options
    removed, and [passes] is a list of passes that were requested by
    a user *)
val run_and_get_passes : string list -> string array -> string array * string list
