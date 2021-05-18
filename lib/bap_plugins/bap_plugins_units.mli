(** Internal module.  *)

open Core_kernel

type reason = [
  | `In_core
  | `Provided_by of string
  | `Requested_by of string
]



(** the name of the selected backend.

    Currently, it should be [findlib] or [dynlink], and is
    selected at configuration time via `./configure --plugins-backend`.
*)
val name : string

(** initializes the unit system.

    May fail if the selected backend is unable to provide safe
    operation.

    Could be only called once per program run.
*)
val init : unit -> unit


(** [list ()] enumerates all currently linked modules.    *)
val list : unit -> string list


(** [record name reason] records unit [name] as well as the
    reason, why it is linked.

    pre: a unit with such name is not linked.
*)
val record : string -> reason -> unit


(** [lookup name] checks if a unit with the given [name] is linked,
    and returns a reason why it was linked. *)
val lookup : string -> reason option

val handle_error : string -> reason -> Dynlink.error -> unit Or_error.t
