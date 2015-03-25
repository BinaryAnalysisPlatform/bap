(** Types for ARM instructions.
     The type definitions itself can be viewed in a
     [Bap_disasm_arm_types] module. This one extends them with a
     [Regular] functions.
*)
open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_arm_types

(** Arm instruction  *)
module Insn : sig
  (** [create basic_insn] lifts ARM instruction from a basic instruction  *)
  val create : ('a,'b) Basic.insn -> insn option

  include module type of Insn
  include Regular with type t := t
end

(** ARM instruction operands  *)
module Op : sig
  type t = Op.t =
    | Reg of reg                (** register *)
    | Imm of word               (** immidiate  *)

  (** lifts operand from a basic one  *)
  val create : Basic.op -> op option
  include Regular with type t := t
end


module Reg : sig
  (** lifts basic register to a ARM one  *)
  val create : Basic.reg -> reg option

  include module type of Reg
  include Regular with type t := t
end

module Cond : sig
  (** decodes condition value from a word  *)
  val create : word -> cond Or_error.t
  include module type of Cond
  include Regular with type t := t
end
