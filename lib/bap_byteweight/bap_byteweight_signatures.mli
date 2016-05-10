(** Provides signatures storage  *)
open Core_kernel.Std
open Regular.Std
open Bap.Std

(** Error conditions  *)
type error = [
  | `Corrupted of string        (** Signature file is corrupted  *)
  | `No_signatures              (** Signature file is not found    *)
  | `No_entry of string         (** Corresponding entry not found  *)
  | `Sys_error of string        (** System error has occured     *)
]


(** [save ?comp ~mode ~path arch data] store signatures data in the
    database of signatures specified by the [path] parameter. The
    triple [arch-comp-mode] defines a key for the created entry. If an
    entry with the same name existed, then it would be overwritten
    with the new data. If the database, doesn't exist, then it will be
    created and the specified destination.*)
val save : ?comp:string -> mode:string -> path:string -> arch -> bytes ->
  (unit,error) Result.t

val load : ?comp:string -> ?path:string -> mode:string -> arch ->
  (bytes,error) Result.t

val default_path : string

val string_of_error : error -> string
