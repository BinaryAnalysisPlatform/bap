open Core_kernel
open Bap_types.Std
open Bap_image_std
open Bap_future.Std
open Bap_core_theory

module Context : sig
  type t
  val for_label : Theory.label -> t KB.t
  val is_applicable : t -> string option -> bool
  val create_addr : t -> unbiased:bool -> Bitvec.t -> addr
end

type 'a t = 'a Or_error.t stream
type 'a source = 'a t

module type Factory = sig
  type t
  val list : unit -> string list
  val find : string -> t source option
  val register : string -> t source -> unit
end

module Factory : sig
  module type S = Factory
  module Make(T : T) : S with type t = T.t
end
