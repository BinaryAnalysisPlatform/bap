
(** Bap-service library.

    {1 Overview}

    The library establishes simple relationships between different
    sides of computation.

    The [service] is an abstraction of resources on which a
    computation may depend.

    Services are provided by service [providers]. The same service
    could be provided by several providers. Only a service provider
    knows how the provided service depends on the environment.

    A [product] is a result of a particular comptutation, that depends
    of computational environment. *)

open Core_kernel.Std
open Bap_future.Std
open Regular.Std

type service
type provider
type product [@@deriving bin_io, compare, sexp]

type void
type literal = (void,void,void) format

module Service : sig
  type t = service

  (** [declare ~desc uuid name] creates a service from description
      [desc], unique id [uuid] and [name] *)
  val declare : desc:string -> uuid:literal -> string -> t

  (** [request service] returns a stream of all products of [service] *)
  val request : t -> product stream
end

module Provider : sig
  type t = provider

  (** [declare ~desc name service] creates a provider of a [service]
      with description [desc] and [name] *)
  val declare : desc:string -> string -> service -> t

  (** [name provider] returns a name of a given [provider] *)
  val name : t -> string

  (** [select ~by_service ~by_name ()] returns a list of providers,
      filtered by name or by service or both of them *)
  val select : ?by_service:service -> ?by_name:string -> unit -> t list
end

module Product : sig
  type t = product  [@@deriving bin_io, compare, sexp]

  (** [provide ~digest provider] issue a new product with [digest] *)
  val provide : digest:string -> provider -> unit

   (** [digest product] returns a digest of a [product] *)
  val digest : t -> string

  (** [provider product] returns a product provider  *)
  val provider : t -> provider

  include Regular.S with type t := t
end
