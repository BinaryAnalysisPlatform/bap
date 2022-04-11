(** Interface to the unified storage of signatures.

    The signatures a key-value pairs (entries) located in one or more
    archives. Keys are target/compiler descriptions and values are
    arbitrary data.

    The data types of the signature are described with the [Data]
    module. This library doesn't specify any data types of signature
    values and they are commonly provided by the libraries that define
    those data types, e.g., [Bap_byteweight.Bytes].
*)

open Core_kernel
open Bap_core_theory
open Bap.Std

(** Error conditions  *)
type error = [
  | `Corrupted of string        (** Signature file is corrupted  *)
  | `No_signatures              (** Signature file is not found    *)
  | `No_entry of string         (** Corresponding entry not found  *)
  | `Sys_error of string        (** System error has occurred     *)
]

(** the descriptor of the data type stored in the signature entry.

    @since 2.5.0
*)
type 'a data


(** [lookup t f] looks up for the matching entry in the signature database.

    The search is performed over the [paths] list that is a list of
    filenames. The first matching entry is selected. If a file in the
    [paths] list doesn't exist then it is skipped. If it exists but
    unreadable an error is returned.

    The paths list is always appended by [[default_path; system_path]],
    in that specific order.

    If [compiler] is specified, then only entries that list matching
    compiler will be selected.

    The target matches are performed with the [Theory.Target.matches]
    function.

    @since 2.5.0
*)
val lookup :
  ?paths:string list ->
  ?compiler:Theory.compiler ->
  Theory.Target.t -> 'a data -> ('a, error) Result.t


(** [update t f x path] updates or creates an entry in the signature database.

    Removes all entries that match with the specified compiler,
    target, and data type and adds a new entry with the provided
    data. All unmatching entries are preserved.

    @since 2.5.0
*)
val update :
  ?compiler:Theory.compiler ->
  Theory.Target.t -> 'a data -> 'a -> string -> (unit,error) Result.t


(** Interface for declaring signature database data types. *)
module Data : sig

  (** [declare ~load ~save name] declares a new mode.

      The [load] and [save] functions are used to store the mode
      information in the signatures database.

      Raises an exception if the mode name is not unique.
  *)
  val declare :
    load:(bytes -> 'a) ->
    save:('a -> bytes) ->
    string -> 'a data
end

(** [save ?comp ~mode ~path arch data] store signatures data in the
    database of signatures specified by the [path] parameter.


    The triple [arch-comp-mode] defines a key for the created entry. If an
    entry with the same name existed, then it would be overwritten
    with the new data. If the database, doesn't exist, then it will be
    created and the specified destination.*)
val save : ?comp:string -> mode:string -> path:string -> arch -> bytes ->
  (unit,error) Result.t
[@@deprecated "since 2022-02 use [lookup]"]

(** [load ?comp ?path ~mode arch] finds a signature for the specified
    [arch-comp-path] triple.

    If [path] is not set, the the signatures are looked up first in
    [default_path] and, if not found, in [system_path].

    Since 2.3.0 the path search has changed to look into two
    locations.
*)
val load : ?comp:string -> ?path:string -> mode:string -> arch ->
  (bytes,error) Result.t
[@@deprecated "since 2022-02 use [update]"]


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
