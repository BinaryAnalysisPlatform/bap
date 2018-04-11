open Bap_future.Std
open Regular.Std

type service
type product
type provider

module Service : sig
  type t = service
  val declare : desc:string -> uuid:string -> string -> t
  val provide : t -> product -> unit
  val request : t -> product stream
end

module Product : sig
  type t = product
  val create : digest:string -> provider -> t
  val digest : t -> string
  val combine : t -> t -> t
  val providers : t -> provider list
end

module Provider : sig
  type t = provider
  val declare : desc:string -> string -> service -> t
  val name : t -> string
  val select : ?by_service:service -> ?by_name:string -> unit -> t list

  include Regular.S with type t := t
end
