(** Server resource.

    Resource is an abstration of a binary data, that can be shared
    between client and server.
*)

open Core_kernel.Std
open Bap.Std
open Rpc


type t with sexp_of


val of_image   : image -> t
val of_section : image:t -> Section.t -> t
val of_symbol  : section:t -> Symbol.t -> t
val of_memory  : ?section:t -> ?symbol:t -> mem -> t

val data : t -> Bigsubstring.t
