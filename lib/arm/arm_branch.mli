open Core_kernel
open Bap.Std
open Arm_types

val lift : op -> ?link:bool -> ?x:bool -> ?cond:op -> word -> stmt list
