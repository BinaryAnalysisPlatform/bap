open Core_kernel
open Format
open Bap_knowledge
open Bap_core_theory

module Lisp : sig
  type program
  type message
  type context

  val program : program knowledge

  val link_program : program -> unit knowledge

  module Load : sig
    type error
    val program : ?paths:string list -> context -> (program,error) result
    val pp_program : formatter -> program -> unit
    val pp_error : formatter -> error -> unit
  end

  module Context : sig
    type t = context
    val empty : t
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

    val check : Sort.exp Var.Ident.Map.t -> program -> error list
    val pp_error : Format.formatter -> error -> unit
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

    val generate_index : index knowledge
  end

  module Message : sig
    type  t = message
    val pp : Format.formatter -> message -> unit
  end

end
