open Core_kernel.Std
open Bap.Std

(** Register representation *)
module type RR = sig
  type t [@@deriving sexp]
  val of_asm : X86_asm.reg -> t option
  val of_asm_exn : X86_asm.reg -> t
  val of_mc : reg -> t option
  val of_mc_exn : reg -> t
  val to_asm : t -> X86_asm.reg
  val width : t -> size
  val var : t -> var
  val size : [`r32 | `r64]
  val get : t -> exp
  val set : t -> exp -> stmt
end

(**Imm model*)
module type IM = sig
  type t
  val of_imm : imm -> t
  val get : width:size -> t -> exp
end

(** Memory model *)
module type MM = sig
  type t
  val of_mem : ?seg:reg -> ?base:reg -> ?scale:imm -> ?index:reg -> disp:imm -> mem -> t
  val of_offset : imm -> t
  val addr : t -> exp
  val addr_size : addr_size
  val load_from : exp -> size:size -> exp
  val store_to : exp -> size:size -> exp -> stmt
  val load : t -> size:size -> exp
  val store : t -> size:size -> exp -> stmt
end

type cpu_flag = [
  | `CF
  | `PF
  | `AF
  | `ZF
  | `SF
  | `DF
  | `OF
]

(** Flags representation *)
module type FR = sig
  type t = cpu_flag
  val var : t -> var
  val get : t -> exp
  val set : t -> exp -> stmt
  val set_unknown : t -> string -> stmt
  val after_sub : diff:exp -> size -> op1:exp -> op2:exp -> stmt list
  val after_add : sum:exp -> size ->  op1:exp -> op2:exp -> stmt list
end

type interrupt_vector = [
  | `DE  (** Divide-by-Zero-Error *)
  | `DB  (** Debug *)
  | `NMI (** Non-Maskable-Interrupt *)
  | `BP  (** Breakpoint *)
  | `OF  (** Overflow *)
  | `BR  (** Bound-Range *)
  | `UD  (** Invalid-Opcode *)
  | `NM  (** Device-Not-Available *)
  | `DF  (** Double-Fault *)
  | `TS  (** Invalid-TSS *)
  | `NP  (** Segment-Not-Present *)
  | `SS  (** Stack *)
  | `GP  (** General-Protection *)
  | `PF  (** Page-Fault *)
  | `MF  (** x87 Floating-Point Exception-Pending *)
  | `AC  (** Alignment-Check *)
  | `MC  (** Machine-Check *)
  | `XF  (** SIMD Floating-Point *)
  | `SX  (** Security Exception  *)
  | `INTR of int (** External Interrupts (Maskable) *)
  | `SOFTWARE of int (** Software Interrupts *)
]

(** Interrupt Vector *)
module type IV = sig
  type t = interrupt_vector

  val to_int : t -> int
end

(** Prefix representation *)
module type PR = sig
  type t = X86_prefix.t

  val lock  : bil -> bil
  val rep   : mem -> bil -> bil
  val repe  : mem -> bil -> bil
  val repne : mem -> bil -> bil
end

module type S = sig
  module RR : RR
  module FR : FR
  module IM : IM
  module MM : MM
  module IV : IV
  module PR : PR
end

module type X86CPU = sig
  type regs = private [< X86_asm.reg]
  val arch : Arch.x86
  val avaliable : X86_asm.reg -> bool
  val cf : var
  val pf : var
  val af : var
  val zf : var
  val sf : var
  val oF : var
  val df : var
  include X86_env.ModeVars
end
