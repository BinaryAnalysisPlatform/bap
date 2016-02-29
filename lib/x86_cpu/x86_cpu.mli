open Bap.Std

module IA32 : sig
  include CPU
  val rbp : var
  val rsp : var
  val rsi : var
  val rdi : var
  val rip : var
  val rax : var
  val rbx : var
  val rcx : var
  val rdx : var
  val r : var array
end


(** [x86-64] architecture  *)
module AMD64 : sig
  include CPU
  include module type of IA32
  val r : var array
end
