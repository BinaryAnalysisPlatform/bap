open Bap.Std
open Bap_powerpc_rtl
open Bap_powerpc_dsl

type cpu = {
  load      : exp -> bitwidth -> exp;
  store     : exp -> exp -> bitwidth -> rtl;
  jmp       : exp -> rtl;
  cia       : exp;       (** current instruction address *)
  addr_size : bitwidth;
  gpr_width : bitwidth;

  (** registers  *)
  reg       : (op -> exp) ec; (** construct exp from register *)
  gpr       : int -> exp; (** general purpose registers 0..31 *)
  fpr       : int -> exp; (** floating-point registers 0..31  *)
  vr        : int -> exp; (** vector register 0..31           *)
  ctr       : exp;       (** count register      *)
  lr        : exp;       (** link register       *)
  tar       : exp;       (** target register     *)
  cr        : exp;       (** condition register  *)
  cr0       : exp;       (** condition register field 0 *)
  cr1       : exp;       (** condition register field 1 *)
  cr2       : exp;       (** condition register field 2 *)
  cr3       : exp;       (** condition register field 3 *)
  cr4       : exp;       (** condition register field 4 *)
  cr5       : exp;       (** condition register field 5 *)
  cr6       : exp;       (** condition register field 6 *)
  cr7       : exp;       (** condition register field 7 *)

  (** fixed precision flags *)
  so        : exp; (** summary overflow        *)
  ca        : exp; (** carry flag              *)
  ov        : exp; (** overflow flag           *)
  ca32      : exp; (** carry out of 32 bits    *)
  ov32      : exp; (** overflow of 32 bits     *)
}


val make_cpu : addr_size -> endian -> mem -> cpu
