(** Interface for the BAP program visitor plugins.

    Plugin is applied to a value of a project type, that can be
    optionally transformed. For example, a plugin can add new
    annotations, symbols or even change re-disassemble program and
    provide their own lifters.
*)
open Bap_types.Std
open Image_internal_std
open Bap_disasm_std

(** The result of Binary analysis.  *)
type project = {
  arch    : arch;               (** architecture  *)
  program : disasm;             (** disassembled memory  *)
  symbols : string table;       (** symbol table  *)
  memory  : mem;                (** base memory *)
  annots  : (string * string) memmap; (** annotations  *)
  bil_of_insns : (mem * insn) list -> bil; (** lifting  *)
}

(** [register plugin] registers [plugin] in the system  *)
val register : (project -> project) -> unit

(** [register' plugin] registers a [plugin] that will be
    evaluated only for side effect. *)
val register': (project -> unit) -> unit

(** A list of registered plugins in the order of registration  *)
val registered : unit -> (project -> project) list
