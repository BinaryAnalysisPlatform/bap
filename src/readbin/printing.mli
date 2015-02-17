open Core_kernel.Std
open Bap.Std

type 'a pp = Format.formatter -> 'a -> unit

module type Env = sig
  (** User options  *)
  val options : Options.t
  (** Base memory region  *)
  val base : mem
  (** Symbol table, to resolve names  *)
  val syms : string table
  (** Control flow graph  *)
  val cfg  : block table
  (** target architecture  *)
  val arch : arch
end
(** Create a set of pretty printers for a given program
    environment.

    Pretty printers also emit appropriate tags, that can
    be controlled using [Tags] interface.
*)
module Make(Env : Env) : sig
  val pp_insns : (mem * insn) seq pp
  val pp_bil : bil pp

  val pp_blk  : (block -> 'a) -> 'a pp -> block pp

  val pp_blk_name : block pp

  val pp_err  : (mem * Disasm.error) pp
  val pp_errs : (mem * Disasm.error) seq pp

  val pp_sym  : block pp -> (mem * string) pp
  val pp_syms : block pp -> string table pp
  val pp_code : 'a pp -> 'a pp
  val pp_concat : 'a pp list -> 'a pp
  val pp_seq  : 'a pp -> 'a seq pp
  val pp_list : ?sep:unit pp -> 'a pp -> 'a list pp
end
