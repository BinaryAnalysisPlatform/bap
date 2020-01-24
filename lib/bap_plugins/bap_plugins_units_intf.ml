(** Internal module.  *)

(** Interface for units table.

    We need to maintain a set of compilation units that comprise the
    program, first of all to prevent double-linking (which is possible
    before 4.08 and will lead to a segfault at best) and second to
    track plugin dependencies that we need and that are not
    satisfied. Even after 4.08, the OCaml loader won't let us link an
    already linked unit (which is not a problem, as they give us
    access to the list of loaded units).

    For now we have an implementation that supports older versions of
    OCaml, which relies on the presence of the META files that
    describe installed packages (we need to map package names (which
    are recorded in the core file by findlib.dynload) to the unit
    names, or on a presence of the `used_<UNIT>` predicates, which are
    recorded by bap building system (or any other build system that is
    capable of passing predicates to ocamlfind, e.g., _not_ dune).

    If META files are not available and units are not recorded in the
    host file via the predicates, then we bail out with an error. This
    could happen, for example, when the host program is built with
    dune (or with some other build system that doesn't record units in
    the predicates) and then the toolchain is erased or otherwise made
    unavailable, e.g., when the program together with plugins is
    packed into a debian package. To be clear, nothing wrong will
    happen with the BAP framework.

    The other implementation, that is available for OCaml versions
    4.08 and above, is totally safe and relies purely on facilities
    provided by the language runtime.
*)

open Core_kernel

type reason = [
  | `In_core
  | `Provided_by of string
  | `Requested_by of string
]


module type S = sig


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
end
