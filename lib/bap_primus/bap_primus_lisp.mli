open Core_kernel
open Bap.Std
open Format
open Bap_primus_types

type program
type message


module Load : sig
  type error
  val program : ?paths:string list -> Project.t -> string list -> (program,error) result
  val pp_program : Format.formatter -> program -> unit
  val pp_error : Format.formatter -> error -> unit
end

module Message : sig
  type  t = message
  val pp : Format.formatter -> message -> unit
end

module Type : sig
  type t
  type signature
  type error

  type parameters = [
    | `All of t
    | `Gen of t list * t
    | `Tuple of t list
  ]

  module Spec : sig
    val any : t
    val var : string -> t
    val sym : t
    val int : t
    val bool : t
    val byte : t
    val word : int -> t
    val a : t
    val b : t
    val c : t
    val d : t

    val tuple : t list -> [`Tuple of t list]
    val all : t -> [`All of t]
    val one : t -> [`Tuple of t list]
    val unit : [`Tuple of t list]
    val (//) : [`Tuple of t list] -> [`All of t] -> parameters
    val (@->) : [< parameters] -> t -> signature
  end

  val check : Var.t seq -> program -> error list
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

val message : message observation


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

  val signal :
    ?params:Type.parameters ->
    ?doc:string ->
    'a observation ->
    ('a -> value list Machine.t) -> unit Machine.t

  val eval_fun : string -> value list -> value Machine.t

  val eval_method  : string -> value list -> unit Machine.t

  (* deprecated *)
  val link_primitives : primitives -> unit Machine.t
end

val init : ?log:formatter -> ?paths:string list -> string list -> unit
