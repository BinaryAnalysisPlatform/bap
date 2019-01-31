open Core_kernel
open Bap.Std
open Arm_types


val tmp : ?name:string -> typ -> var

val assn : var -> exp -> stmt

val fail : Source_code_position.t -> ('a,unit,string,'b) format4 -> 'a

val bitlen : typ -> int

val exec : stmt list -> ?flags:stmt list -> ?wflag:op -> op -> stmt list

val exp_of_op : op -> exp

val exp_of_reg : reg -> exp

val cast_of_sign : sign -> int -> exp -> exp


val assert_reg : Source_code_position.t -> op -> reg

val assert_imm : Source_code_position.t -> op -> word

val assert_cond : Source_code_position.t -> op -> cond

val msb : exp -> exp

val zero : typ -> exp
