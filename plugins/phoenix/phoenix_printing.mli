open Core_kernel.Std
open Bap.Std

type 'a pp = Format.formatter -> 'a -> unit

(** Create a set of pretty printers for a given program
    environment.

    Pretty printers also emit appropriate tags, that can
    be controlled using [Tags] interface.
*)
module Make(Env : sig val project : project end) : sig
  val pp_insns : (mem * insn) list pp
  val pp_bil : bil pp

  val pp_blk  : (block -> 'a) -> 'a pp -> block pp

  val pp_blk_name : block pp

  val pp_sym  : block pp -> Symtab.fn pp
  val pp_syms : block pp -> Symtab.t pp
  val pp_code : 'a pp -> 'a pp
  val pp_concat : ?sep:unit pp -> 'a pp list -> 'a pp
  val pp_seq  : 'a pp -> 'a seq pp
  val pp_list : ?sep:unit pp -> 'a pp -> 'a list pp
end
