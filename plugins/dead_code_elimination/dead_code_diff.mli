open Core_kernel
open Bap.Std
open Regular.Std

type t

(** [create prg subs] creates diff of subroutines of the program [prg] and [subs] *)
val create : program term -> sub term list -> t

(** [apply prg diff] applies diff to [prg]  *)
val apply : program term -> t -> program term

include Regular.S with type t := t
