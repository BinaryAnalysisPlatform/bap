open Bap.Std

exception Plugin_not_found of string

(** [run argv] extracts and loads plugins from a command line
    arguments, specified by [argv] parameter.  Returns an array of
    command line arguments with plugin specific options removed. *)
val run : string array -> string array

(** [run_and_get_passes argv] loads plugins specified in the command line arguments
    passed as [argv] and returns [argv,passes], where [argv] is an
    array of command line arguments with all plugin specific options
    removed, and [passes] is a list of passes that were requested by
    a user *)
val run_and_get_passes : string array -> string array * string list
