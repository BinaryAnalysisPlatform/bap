open Core_kernel
open Bap.Std
open Format
open Bap_primus_types
open Bap_core_theory

type program
type message


module Load : sig
  type error
  val program : ?paths:string list -> Project.t -> string list -> (program,error) result
  val pp_program : formatter -> program -> unit
  val pp_error : formatter -> error -> unit
end

module Doc : sig
  module type Element = sig
    type t
    val pp : formatter -> t -> unit
  end

  module Category : Element
  module Name     : Element
  module Descr    : Element
  type index = (Category.t * (Name.t * Descr.t) list) list

  module Make(Machine : Machine) : sig
    val generate_index : index Machine.t
  end
end

module Message : sig
  type  t = message
  val pp : Format.formatter -> message -> unit
end

module Type : sig
  type t
  type env
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

  val error : error observation

  val errors : env -> error list
  val check : Var.t seq -> program -> error list
  val pp_error : Format.formatter -> error -> unit
end

module Closure : sig
  module type S = functor(Machine : Machine) -> sig
    val run : value list -> value Machine.t
  end

  type t = (module S)

  module Make(Machine : Machine) : sig
    val name : string Machine.t
  end
end
module type Closure = Closure.S

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

val primitive : (string * value list) observation

module Make (Machine : Machine) : sig
  val failf : ('a, unit, string, unit -> 'b Machine.t) format4 -> 'a

  val link_program : program -> unit Machine.t

  val program : program Machine.t

  val typecheck : unit Machine.t

  val types : Type.env Machine.t

  val define : ?types:Type.signature -> ?docs:string -> string -> closure -> unit Machine.t

  val signal :
    ?params:[< Type.parameters] ->
    ?doc:string ->
    'a observation ->
    ('a -> value list Machine.t) -> unit Machine.t

  val eval_fun : string -> value list -> value Machine.t

  val eval_method  : string -> value list -> unit Machine.t

  val optimize : unit -> unit Machine.t

  val refine : Bap_primus_lisp_context.t -> unit Machine.t

  (* deprecated *)
  val link_primitives : primitives -> unit Machine.t
end

module Semantics : sig
  type primitive

  module Primitive : sig
    type t = primitive
    val name : t -> string
    val args : t -> unit Theory.Value.t list
  end

  val program : (Theory.Source.cls, program) KB.slot
  val primitive : (Theory.program, primitive option) KB.slot
  val symbol : (Theory.Value.cls, String.t option) KB.slot
  val static : (Theory.Value.cls, Bitvec.t option) KB.slot
  val enable : unit -> unit
end

module Unit : sig
  val create : ?name:string -> Theory.Target.t -> Theory.Unit.t KB.t
  val is_lisp : Theory.Unit.t -> bool KB.t
  val language : Theory.language
end

val init : ?log:formatter -> ?paths:string list -> string list -> unit
