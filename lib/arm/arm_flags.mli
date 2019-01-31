open Core_kernel
open Bap.Std
open Arm_types

val set_nzf : exp -> typ -> stmt list

val set_vnzf_add : exp -> exp -> exp -> typ -> stmt list

val set_add : exp -> exp -> exp -> typ -> stmt list

val set_sub : exp -> exp -> exp -> typ -> stmt list

val set_vnzf_sub : exp -> exp -> exp -> typ -> stmt list

val set_adc : exp -> exp -> exp -> typ -> stmt list

val set_sbc : exp -> exp -> exp -> typ -> stmt list

val set_cf_data : imm:word -> data:word -> stmt
