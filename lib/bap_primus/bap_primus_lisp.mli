open Core_kernel
open Bap.Std
open Format
open Bap_primus_types

type program

val message : string observation

module Load : sig
  type error
  val program : ?paths:string list -> Project.t -> string list -> (program,error) result
  val pp_program : Format.formatter -> program -> unit
  val pp_error : Format.formatter -> error -> unit
end

module Type : sig
  type t
  type signature
  type error
  val word : int -> t
  val any : t
  val signature : ?rest:t -> t list -> t -> signature
  val check : Var.t  seq -> program -> error list
  val pp_error : Format.formatter -> error -> unit
end

module type Closure = functor(Machine : Machine) -> sig
  val run : value list -> value Machine.t
end

type closure = (module Closure)

module Primitive : sig
  type 'a t
  val create : ?docs:string -> string -> (value list -> 'a) -> 'a t
end

module type Primitives = functor (Machine : Machine) ->  sig
  val defs : unit -> value Machine.t Primitive.t list
end

type primitives = (module Primitives)
type exn += Runtime_error of string

module Make (Machine : Machine) : sig
  val failf : ('a, unit, string, unit -> 'b Machine.t) format4 -> 'a

  val link_program : program -> unit Machine.t

  val program : program Machine.t

  val define : ?types:Type.signature -> ?docs:string -> string -> closure -> unit Machine.t

  (* deprecated *)
  val link_primitives : primitives -> unit Machine.t
end

val init : ?log:formatter -> ?paths:string list -> string list -> unit
