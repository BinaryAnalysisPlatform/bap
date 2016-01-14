(** Extends [size] type.  *)

open Core_kernel.Std
open Bap_common

(** {2 Lifting from int} *)

(** [of_int n] return [Ok `rn] if [`rn] exists, [Error]
    otherwise.  *)
val of_int : int -> size Or_error.t

(** [of_int_exn n] the same as [of_int], but raises exception
    instead of returning [Error] *)
val of_int_exn : int -> size

(** [of_int_opt n] the same as [of_int] but uses [option] type
    instead of [Or_error.t] *)
val of_int_opt : int -> size option

(** [addr_of_int n] return [Ok `rn] if [`rn] exists, [Error]
    otherwise.  *)
val addr_of_int : int -> addr_size Or_error.t

(** [addr_of_int_exn n] the same as [addr_of_int], but raises exception
    instead of returning [Error] *)
val addr_of_int_exn : int -> addr_size

(** [addr_of_int_opt n] the same as [addr_of_int] but uses [option] type
    instead of [Or_error.t] *)
val addr_of_int_opt : int -> addr_size option

val addr_of_word_size : Word_size.t -> addr_size

val word_of_addr_size : addr_size -> Word_size.t

val to_addr_size : size -> addr_size Or_error.t

(** [to_bits size] returns the number of bits. *)
val in_bits  : 'a Size.p -> int
val in_bytes : 'a Size.p -> int

include Regular with type t := size
