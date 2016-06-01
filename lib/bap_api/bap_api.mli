(** An interface to the api pass.

    Api pass will apply a high level model of a program [api] to a low
    level representation, i.e., the [program term].

    The module is actually a static registry of language processors,
    that will be applied by the api pass.

    The language processors are provide by plugins supporting
    corresponding languages. Sometimes an abi information is required
    to apply the api, so the processors are actually provided by even
    lower level modules, that supports specific architectures.

*)

open Core_kernel.Std
open Bap.Std

type filename = string
type api = string

(** Language processor interface.  *)
module type S = sig

  type t

  (** [language] a name of a language, e.g., ["C"] *)
  val language : string

  (** [parse get_api apis] creates a language processor from a list of
      api. Function [get_api api] must return a name of an existing
      file, that corresponds to the given [api]. The [apis] parameter
      is a list of [api] names.  *)
  val parse : (api -> filename option) -> api list -> t Or_error.t

  (** the processor itself  *)
  val mapper : t  -> Term.mapper
end

(** language processor type  *)
type t = (module S)

(** apply the language processor  *)
val process : t -> unit

(** enumerate all registered language processors  *)
val processors : unit -> t list
