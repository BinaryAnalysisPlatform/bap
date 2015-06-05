(** ELF container backend *)
open Core_kernel.Std
open Bap_types.Std
open Image_backend

val of_data : Bigstring.t -> Img.t option
