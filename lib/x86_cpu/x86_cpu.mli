open Bap.Std


(** IA32 Architecture Registers.

    For simplicity we're using the same names for registers in
    32 and 64 mode. For example, the A register, has a name [rax] on
    both 32-bit and 64-bit processors. However, on the former it is
    32-bit (contrary to the name), and on the latter it is 64-bit.
  *)
module IA32 : sig
  include Bap.Std.CPU


  (** [flags] is a set of flag registers  *)
  val flags : Var.Set.t

  (** base pointer  *)
  val rbp : var

  (** stack pointer  *)
  val rsp : var

  (** source index  *)
  val rsi : var

  (** destination index  *)
  val rdi : var

  (** instruction pointer  *)
  val rip : var

  (** accumulator register  *)
  val rax : var

  (** base register  *)
  val rbx : var

  (** counter register  *)
  val rcx : var

  (** data register  *)
  val rdx : var

  (** YMM registers that are available *)
  val ymms: var array

end


(** AMD64 registers  *)
module AMD64 : sig
  include Bap.Std.CPU
  include module type of IA32

(** r8-r15 registers.
      Due to a legacy issues r.(0) -> r8, r.(1) -> r8, ... *)
  val r : var array
end
