open Core_kernel.Std
open Bap.Std
open Arm_types


(** [spsr]  Saved Processor Status Register *)
val spsr : var


(** [cpsr] Current Processor Status Register  *)
val cpsr : var


(** [nf] Negative Flag  *)
val nf : var

(** [zf] Zero Flag  *)
val zf : var

(** [cf] Carry Flag  *)
val cf : var

(** [vf] oVerfrlow Flag  *)
val vf : var

(** [qf] underflow (saturation) Flag  *)
val qf : var


(** [ge] array of general registers  *)
val ge : var array


(** [itstate] ITSTATE register  *)
val itstate : var


(** [lr] Link Register   *)
val lr : var


(** [pc] Program Counter  *)
val pc : var

(** [sp] Stack Pointer  *)
val sp : var


(** general purpose register  *)
val r0 : var

(** general purpose register  *)
val r1 : var

(** general purpose register  *)
val r2 : var

(** general purpose register  *)
val r3 : var

(** general purpose register  *)
val r4 : var

(** general purpose register  *)
val r5 : var

(** general purpose register  *)
val r6 : var


(** general purpose register  *)
val r7 : var


(** general purpose register  *)
val r8 : var


(** general purpose register  *)
val r9 : var

(** general purpose register  *)
val r10 : var

(** general purpose register  *)
val r11 : var

(** general purpose register  *)
val r12 : var


(** [of_reg arm_reg] lifts arm register into BIL variable  *)
val of_reg : reg -> var


(** [new_var name] creates a freshly new variable prefixed with [name]  *)
val new_var : string -> var


(** [mem] BIL variable that denotes the system memory.  *)
val mem : var
