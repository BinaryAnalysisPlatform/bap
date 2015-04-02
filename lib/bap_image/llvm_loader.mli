open Core_kernel.Std
open Bap_types.Std
open Image_backend

val from_data : Bigstring.t -> Img.t option

val from_file : string -> Img.t option
