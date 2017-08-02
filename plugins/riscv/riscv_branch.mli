open Core_kernel.Std
open Bap.Std
open Riscv_types

val lift : op -> op -> ?cond:op -> ?link:bool -> word -> stmt list
