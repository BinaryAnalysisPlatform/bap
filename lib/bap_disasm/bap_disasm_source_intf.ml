open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_future.Std
open Bap_service

type 'a t = 'a Or_error.t stream
type 'a source = 'a t

module type Factory = sig
  type t
  val list : unit -> string list
  val find : string -> t source option

  val register : string -> t source -> unit
  [@@deprecated "[since 2018-04] in favor of provide"]

  val provide : provider -> t source -> unit
  val request : provider -> t source option
  val providers : unit -> provider list
end
