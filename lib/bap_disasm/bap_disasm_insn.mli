(** First class instruction.

    This module provides instruction as a first class value,
    with all optimizations turned off. It is a part of simplified
    interface intended for end-users.

    For more compilicated, but more efficient interface, see
    [Basic.insn] and [Rec.decoded] types.
*)
open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_types

type t with bin_io, compare, sexp

type op = Op.t with bin_io, compare, sexp

include Regular with type t := t

(** returns backend specific name of instruction *)
val name : t -> string

(** target-specific assembler string representing the instruction  *)
val asm  : t -> string

(** returns BIL program specifying instruction semantics  *)
val bil  : t -> bil

(** instruction operands  *)
val ops  : t -> op array

(** {3 Instruction predicates} *)

(** [is_jump] [true] for all jumps  *)
val is_jump : t -> bool

(** [is_conditional] [true] for conditional jumps  *)
val is_conditional_jump : t -> bool

(** [is_unconditional] iff [is_jump && not is_conditional_jump]  *)
val is_unconditional_jump : t -> bool

(** [is_indirect_jump] [true] if it is indirect *)
val is_indirect_jump : t -> bool

(** [is_call] is [true] for all call instructions  *)
val is_call : t -> bool

(** [is_return] [true] for returns  *)
val is_return : t -> bool

(** [may_affect_control_flow] is true if it may affect control flow.
    «may» stays for the fact, that it «may not» affect.
*)
val may_affect_control_flow : t -> bool

(** [has_side_effect] is [true] if instruction may load or store  *)
val has_side_effect : t -> bool

(** [may_load] is true if instruction may load data from memory  *)
val may_load : t -> bool

(** [may_store] is true if instruction may store data to memory  *)
val may_store : t -> bool


(** {3 Prefix Tree}
    This module provides a trie data structure where a sequence of
    instructions is used as a key (and an individual instruction
    as a token). Two implementations are provided, a regular, where
    insns are compared as-is, and normalized, where instructions are
    compared using normalized comparison.

    In normalized comparison concerete immediate values are ignored,
    and if instructions have different number of operands, then only
    then excess operands are excluded from the comparison.
*)
module Trie : sig
  (** Trie requires 0(1) get operation  *)
  type key

  (** [key_of_insns insns] takes a list of instructions and transforms
      it to [key] *)
  val key_of_insns : t list -> key

  module Normalized : Trie with type key = key
  include Trie with type key := key
end


(** {3 Creating}
    The following functions will create [insn] instances from a lower
    level representation.
*)
val of_basic : ?bil:bil -> Basic.full_insn -> t
