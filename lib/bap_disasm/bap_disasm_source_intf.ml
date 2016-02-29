open Core_kernel.Std
open Bap_types.Std
open Bap_image_std

type 'a t =
  | File   : string t
  | Binary : image t
  | Memory : (mem * arch) t

type 'a source = 'a t

module type Factory = sig
  type t
  val list : 'a source -> string list
  val find : 'a source -> string -> 'a -> t option
  val register : 'a source -> string -> ('a -> t option) -> unit
end
