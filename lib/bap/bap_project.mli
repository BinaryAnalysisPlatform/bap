open Core_kernel.Std
open Bap_types.Std
open Image_internal_std
open Bap_disasm_std
open Bap_sema.Std

(** The result of Binary analysis.  *)
type t = {
  arch    : arch;               (** architecture  *)
  disasm  : disasm;             (** disassembly of a program *)
  memory  : value memmap;       (** annotations  *)
  storage : dict;               (** arbitrary data storage *)
  program : program term;       (** program lifted to BAP IR *)
  (** Deprecated fields, the will be removed in a next release. *)
  symbols : string table;       (** @deprecated symbol table  *)
  base    : mem;                (** @deprecated base memory  *)
}

(** all string tags supports the following substitutions:

    - $region_{name,addr,min_addr,max_addr} - name of region of file
      to which it belongs. For example, in ELF this name will
      correspond to the section name

    - $symbol_{name,addr} - name or address of the symbol to which this
      memory belongs

    - $asm - assembler listing of the memory region

    - $bil - BIL code of the tagged memory region

    - $block_{name,addr} - name or address of a basic block to which
      this region belongs

    - $min_addr, $addr - starting address of a memory region

    - $max_addr - address of the last byte of a memory region.
*)
val substitute : ?tags:string tag list -> t -> t


(** [register plugin] registers [plugin] in the system  *)
val register_plugin : (t -> t) -> unit

(** [register' plugin] registers a [plugin] that will be
    evaluated only for side effect. *)
val register_plugin': (t -> unit) -> unit

val register_plugin_with_args : (string array -> t -> t) -> unit

val register_plugin_with_args' : (string array -> t -> unit) -> unit

(** A list of registered plugins in the order of registration  *)
val plugins : unit -> (string array -> t -> t) list
