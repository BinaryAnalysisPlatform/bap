open Core_kernel.Std
open Bap.Std

open Mips_rtl
open Mips_dsl
open Mips_utils
open Mips_model

type cpu = {
    load : exp -> bitwidth -> exp;
    store : exp -> exp -> bitwidth -> rtl;
    jmp : exp -> rtl;
    cia : exp;
    addr_size : bitwidth;
    gpr_width : bitwidth;
    reg : (op -> exp) ec;
    gpr : int -> exp;
    fpr : int -> exp;
}

