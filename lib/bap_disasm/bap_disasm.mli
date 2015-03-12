(** High-level interface to the disassembler.

    This module is designed in assumptions that it is opened before
    usage. If you open [Bap.Std] as prescribed, all defintions from
    this module will be available for you without further
    qualifications.
*)

open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_types
open Image_internal_std


(** value of type [disasm] is a result of the disassembling of a
    memory region. To create values of this type use [disassemble]
    function *)
type disasm

(** values of type [insn] represents machine instructions decoded
    from the given piece of memory *)
type insn = Bap_disasm_insn.t with bin_io, compare, sexp_of
type op = Op.t with bin_io, compare, sexp_of

(** [block] is a region of memory that is believed to be a basic block
    of control flow graph to the best of our knowledge. *)
type block = Bap_disasm_block.t with compare, sexp_of

(** [disassemble ?roots arch mem] disassemble provided memory region
    [mem] using best available algorithm and backend for the specified
    [arch]. Roots, if provided, should point to memory regions, that
    are believed to contain code. At best, this should be a list of
    function starts. If no roots are provided, then the starting
    address of the provided memory [mem] will be used as a root.

    The returned value will contain all memory reachable from the
    given set of roots, at our best knowledge. *)
val disassemble : ?roots:addr list -> arch -> mem -> disasm

(** [disassemble_image image] disassemble given image.
    Will take executable sections of the image and disassemble it,
    applying [disassemble] function. If no roots are specified, then
    symbol table will be used as a source of roots. If file doesn't
    contain one, then entry point will be used.
*)
val disassemble_image : ?roots:addr list -> Bap_image.t -> disasm

(** [disassemble_file ?roots path] takes a path to a binary and
    disassembles it  *)
val disassemble_file : ?roots:addr list -> string -> disasm Or_error.t

(** [disassemble_file ?roots path] takes a path to a binary and
    disassembles it  *)
val disassemble_file_exn : ?roots:addr list -> string -> disasm

(** [linear_sweep arch mem] will perform a linear sweep disassembly on
    the specified memory [mem] *)
val linear_sweep : arch -> mem -> (mem * insn option) list Or_error.t
val linear_sweep_exn : arch -> mem -> (mem * insn option) list


(** Disassembled program  *)
module Disasm : sig
  type t = disasm

  (** returns all instructions that was successfully decoded in an
      ascending order of their addresses. Each instruction is
      accompanied with its block of memory. *)
  val insns : t -> (mem * insn) seq

  (** returns a mapping from memory regions to basic blocks  *)
  val blocks : t -> block table

  (** performs fast lookup for an instruction that occupies exactly
      the given piece of memory. If you need to find all instructions
      that lies in a given region of memory, use [insns_of_mem] or
      [insns_of_block] functions.
  *)
  val insn_at_mem : t -> mem -> insn option

  (** [insns_of_mem] returns all instructions that occupies memory
      regions that have intersections with [mem].  *)
  val insns_at_mem : t -> mem -> (mem * insn) seq

  (** returns a sequence of memory regions occupied by the given
      instruction.  *)
  val mems_of_insn : t -> insn -> mem seq

  (** [insn_at_addr t addr] finds instruction to which the [addr]
      belongs. In other words if instruction at given address is
      found it doesn't mean, that it starts at this address.
      Consider comparison with [min_addr] if you need to match
      starting address only.  *)
  val insn_at_addr : t -> addr -> (mem * insn) option


  (** returns a blocks of memory that was visited during the
      disassembly. The regions are merged with [Memory.merge] if
      possible. So it returns the least possible amount of contiguous
      memory regions *)
  val span : t -> mem seq

  type error = [
    | `Failed of Error.t
    | `Failed_to_disasm of mem
    | `Failed_to_lift of mem * insn * Error.t
  ] with sexp_of

  module Error : Printable with type t := error

  (** returns a list of all errors and warnings that occurred during
      the disassembling *)
  val errors : t -> (mem * error) seq
end
