open Bap.Std
open Primus_types


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

val init : ?paths:string list -> string list -> unit
