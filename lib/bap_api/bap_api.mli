open Core_kernel.Std
open Bap.Std

type filename = string
type api = string


module type S = sig
  type t
  val language : string
  (** [parse get_api apis] creates a language processor from a list of
      api. Function [get_api api] must return a name of an existing
      file, that corresponds to the given [api]. The [apis] parameter
      is a list of [api] names.  *)
  val parse : (api -> filename option) -> api list -> t Or_error.t
  val mapper : t  -> Term.mapper
end

type t = (module S)
type id

val process : t -> id
val retract : id -> unit
val processors : unit -> t list
