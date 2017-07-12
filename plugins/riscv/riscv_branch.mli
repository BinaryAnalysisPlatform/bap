open Core_kernel.Std
open Bap.Std
open Riscv_types

val lift : op -> ?link:bool -> ?cond:op -> word -> stmt list
