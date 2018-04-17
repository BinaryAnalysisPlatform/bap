
(** Bap-service library.

    {1 Overview}

    The library establishes simple relationships between different
    sides of computation.

    The [service] is an abstraction of resources on which a
    computation may depend.

    Services are provided by service [providers]. The same service
    could be provided by several providers. Only a service provider
    knows how the provided service depends on the environment.

    A [product] is a particular comptutation, that depends
    of computational environment. *)

open Bap_future.Std
open Regular.Std

type service
type product
type provider

module Service : sig
  type t = service

  (** [declare ~desc uuid name] creates a service from description
      [desc], unique id [uuid] and [name] *)
  val declare : desc:string -> uuid:string -> string -> t

  (** [provide service product] supplies a [service] with a [product] *)
  val provide : t -> product -> unit

  (** [request service] returns a stream of all products of [service] *)
  val request : t -> product stream

  include Regular.S with type t := t
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

  include Regular.S with type t := t
end

module Product : sig
  type t = product

  (** [create ~digest provider] creates a product for [proivder]
      with [digest], where the latter is a digest of computational
      environment *)
  val create : digest:string -> provider -> t

  (** [digest product] returns a digest of a [product] *)
  val digest : t -> string

  (** [combine p p'] returns a sum of two products *)
  val combine : t -> t -> t

  (** [providers product] returns a list of providers that
      participates in creation of a given [product]  *)
  val providers : t -> provider list

  include Regular.S with type t := t
end
