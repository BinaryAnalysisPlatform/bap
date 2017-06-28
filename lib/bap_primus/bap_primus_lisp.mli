open Bap.Std
open Format
open Bap_primus_types


(** Lisp machine.

    Lisp machine is a Primus component that provides a facility to
    link programs written in a Primus Lisp with the emulated programs.

    It is useful for writing stubs, especially, for library
    functions. For example, if a program has a call to [strcpy]
    function that is dynamically provided by a [libc] library, then it
    can be stubbed with the following lisp code:

    [{
    (defun strcpy (dst src)
      (declare (external "strcpy"))
      (let ((dst dst))
        (while (/= (points-to-null p))
          (copy-byte-shift dst src))
        (memory-write dst 0:8))
      dst)
    }]

    {2 Primus Lisp Primer}

    Primus Lisp is a dialect of Lisp close to Common Lisp in
    syntax, that can be considered as a frontend to BIL. The main
    feature of the language, is that it has only one data type - the
    machine word. No lists, functions, or anything like this. This
    basically puts the language into the family of assembler
    languages. However, there are few high-level features that work on
    syntax or linkage stage, that makes the language much more
    powerful an expressive. First of all it is the macro system, that
    allows one to abstract over any syntactic term, so that it is
    possible to write abstractions over functions or even
    types. The second feature, extends Primus Lisp with the ad-hoc
    polymorphism or overloading. The system is similar to Haskell's
    type classes or Rust's traits.


*)

(** a primitive that can be called from lisp code  *)
module Primitive : sig
  type 'a t
  val create : ?docs:string -> string -> (word list -> 'a) -> 'a t
end

module type Primitives = functor (Machine : Machine) ->  sig

  (** each primitive is an OCaml function that takes a list of words
      and returns a word. A primitive linkage is internal to the Lisp
      machine, so it is visible only to Lisp functions.
      If you want to implement a stub function in OCaml, then you
      should work directly with the Linker module. The primitives
      extend only the Lisp machine.
  *)
  val defs : unit -> Word.t Machine.t Primitive.t list
end

type primitives = (module Primitives)


(** Creates a Lisp machine.

    You can extend the machine with primitive functions implemented in
    OCaml.
*)

type exn += Runtime_error of string

module Make (Machine : Machine) : sig
  val failf : ('a, unit, string, unit -> 'b Machine.t) format4 -> 'a
  val link_primitives : primitives -> unit Machine.t
end


(** [init ~paths features] initialize the Lisp machine, load all
    [features] looking in the specified set of paths, and register a
    Primus machine component that will use the Linker component to
    link into the program all definitions with the external linkage
    (i.e., those with the external attribute).

    The function should be called only once, by a component that is
    responsible for Lisp machine initialization. By default it is the
    lisp_library_plugin. The function will raise an exception if it is
    called twice.
*)
val init : ?log:formatter -> ?paths:string list -> string list -> unit
