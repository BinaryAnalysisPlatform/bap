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

(** target-specific assembler string representing the instructio  *)
val asm  : t -> string

(** returns BIL program specifying instruction semantics  *)
val bil  : t -> bil

(** instruction operands  *)
val ops  : t -> op array

(** {3 Creating}
    The following functions will create [insn] instances from a lower
    level representation.
*)
val of_basic : ?bil:bil -> Basic.full_insn -> t
val of_decoded : Rec.decoded -> t option
