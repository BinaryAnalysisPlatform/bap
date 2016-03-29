open Core_kernel.Std
open Regular_data_intf
open Format

type 'a t

val create :
  load:(digest -> 'a option) ->
  save:(digest -> 'a -> unit) -> 'a t

val digest : namespace:string -> ('a,formatter,unit,digest) format4 -> 'a

val load : 'a t -> digest -> 'a option

val save : 'a t -> digest -> 'a -> unit

type service = {
  create : 'a . 'a reader -> 'a writer -> 'a t
}

module Service : sig
  val provide : service -> unit
  val request : 'a reader -> 'a writer -> 'a t
end

module Digest : sig
  type t = digest
  val create : namespace:string -> t
  val add : t -> ('a,formatter,unit,t) format4 -> 'a
  val add_sexp : t -> ('a -> Sexp.t) -> 'a -> t
  val add_file : t -> string -> t
  include Identifiable with type t := t
end
