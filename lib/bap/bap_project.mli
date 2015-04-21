(** Interface for the BAP project.

    Project represents the results of analysis applied to a certain
    binary object. It is slightly more abstract, then binary image,
    thus allowing to apply analysis on arbitrary binary blobs.

    Project is also a common ground, where different plugins can meet,
    talk and exchange with information. Each plugin is barely a
    function of type [t -> t], i.e., it can functionally update
    project, e.g., add annotations, discover new symbols, remove
    incorrect and, even, change architecture and redisassemble
    everything.


    {3 Exchanging information}

    For exchanging information in type safe manner we use universal
    values. Values can be attached to a particular memory region, or to
    a key of type [string]. For the first case we use `memmap` data
    structure that is an interval tree containing all the memory
    regions that were used during analysis. For the latter a simple
    [String.Map] is used.


    {4 Annotating memory}

    Depending on the analysis performed and input parameters, one can
    expect that memory may be annotated with the following tags:

    - [Image.region] -- for regions of memory, that had some
      particular name in the original binary. For example, in ELF
      sections have names, that will be used to annotate corresponding
      memory regions.

    - [Image.section] -- if the binary data was loaded from a binary
      format that contains sections (aka segments), then corresponding
      memory regions would be marked. Sections gives you access

*)
open Core_kernel.Std
open Bap_types.Std
open Image_internal_std
open Bap_disasm_std

(** The result of Binary analysis.  *)
type t = {
  arch    : arch;               (** architecture  *)
  disasm  : disasm;             (** disassembly of a program *)
  memory  : value memmap;       (** annotations  *)
  storage : value String.Map.t; (** arbitrary data storage *)

  (** Deprecated fields, the will be removed in a next release. *)
  symbols : string table;       (** symbol table  @deprecated *)
  base    : mem;                (** base memory   @deprecated *)
}

type color = [
  | `black
  | `red
  | `green
  | `yellow
  | `blue
  | `magenta
  | `cyan
  | `white
] with sexp

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


(** an arbitrary text *)
val text : string tag

(** the associated data is an html markup *)
val html : string tag

(** associate a comment string with a memory region  *)
val comment : string tag

(** to assosiate a python command with a region *)
val python : string tag

(** to assosiate a shell command with a region  *)
val shell : string tag

(** just mark a region  *)
val mark : unit tag

(** attach a color  *)
val color : color tag

(** attach a weight to a memory  *)
val weight : float tag

(** [register plugin] registers [plugin] in the system  *)
val register_plugin : (t -> t) -> unit

(** [register' plugin] registers a [plugin] that will be
    evaluated only for side effect. *)
val register_plugin': (t -> unit) -> unit

val register_plugin_with_args : (string array -> t -> t) -> unit

val register_plugin_with_args' : (string array -> t -> unit) -> unit

(** A list of registered plugins in the order of registration  *)
val plugins : unit -> (string array -> t -> t) list
