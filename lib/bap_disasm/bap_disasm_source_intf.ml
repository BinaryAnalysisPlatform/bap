open Core_kernel
open Bap_types.Std
open Bap_image_std
open Bap_future.Std

type 'a t = 'a Or_error.t stream
type 'a source = 'a t

module type Factory = sig
  type t
  val list : unit -> string list
  val find : string -> t source option
  val register : string -> t source -> unit
end
