open Core_kernel.Std
open Bap.Std

(** IA32 target *)
module IA32 : Target

(** AMD64 target *)
module AMD64 : Target


(** IA32 legacy target *)
module IA32L : Target

(** AMD64 legacy target *)
module AMD64L : Target

(** IA32 target has been merged with legacy lifter *)
module IA32M : Target

(** AMD64 target has been merged with legacy lifter *)
module AMD64M : Target
