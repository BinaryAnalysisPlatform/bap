open Core_kernel
open Bap_common
open Bap_bil

val check : stmt list -> (unit,Bap_type_error.t) Result.t
val infer : exp -> (typ, Bap_type_error.t) Result.t
val infer_exn : exp -> typ
