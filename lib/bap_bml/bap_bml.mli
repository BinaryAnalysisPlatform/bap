open Core_kernel
open Bap.Std

(** BML - Bap Mapping Language.

    A small DSL for mapping terms. See [bap --map-terms-help] for full
    language grammar and description. This library implements BML's
    standard library and can be used to extend the language with new
    predicates and mapper. Just implement them and register using
    corresponding module.
*)


(** A parser error  *)
exception Parse_error of string

(** Interface to a registry.
    Registry is a key value storage.*)
module type Registry = sig
  type t

  (** [register name value] register [value] with a given [name].
      If [name] was already associated with some other value, then
      it will be superseded with the new binding.   *)
  val register : string -> t -> unit

  (** [find name] find a value associated with the given [value]  *)
  val find : string -> t option

  (** [list ()] list all bindings  *)
  val list : unit -> (string * t) list
end

module type Ops = sig
  type t
  module Nullary  : Registry with type t = t
  module Unary    : Registry with type t = string -> t
end

module Predicates : Ops with type t = bool Term.visitor
module Mappers    : Ops with type t = Term.mapper
