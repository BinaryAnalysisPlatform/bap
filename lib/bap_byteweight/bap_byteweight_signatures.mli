(** Provides signatures storage  *)
open Core_kernel
open Regular.Std
open Bap.Std

(** Error conditions  *)
type error = [
  | `Corrupted of string        (** Signature file is corrupted  *)
  | `No_signatures              (** Signature file is not found    *)
  | `No_entry of string         (** Corresponding entry not found  *)
  | `Sys_error of string        (** System error has occurred     *)
]


(** [save ?comp ~mode ~path arch data] store signatures data in the
    database of signatures specified by the [path] parameter. The
    triple [arch-comp-mode] defines a key for the created entry. If an
    entry with the same name existed, then it would be overwritten
    with the new data. If the database, doesn't exist, then it will be
    created and the specified destination.*)
val save : ?comp:string -> mode:string -> path:string -> arch -> bytes ->
  (unit,error) Result.t


(** [load ?comp ?path ~mode arch] finds a signature for the specified
    [arch-comp-path] triple.

    If [path] is not set, the the signatures are looked up first in
    [default_path] and, if not found, in [system_path].

    Since 2.3.0 the path search has changed to look into two
    locations.
*)
val load : ?comp:string -> ?path:string -> mode:string -> arch ->
  (bytes,error) Result.t


(** default path for the user's signatures database.

    Since 2.3.0 it is pointed to the user-specific location, not
    to the system-wide. See also {!system_path}.
*)
val default_path : string

(** the path to the system-wide location of signatures.

    @since 2.3.0*)
val system_path : string

(** a human readable representation of an error.  *)
val string_of_error : error -> string
