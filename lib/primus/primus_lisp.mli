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
(* module Machine(Machine : Machine) : sig *)
(*   (\** [add_directory dirname] adds [dirname] to the list of *)
(*       directories with lisp modules. If different directories provide *)
(*       features with the same name, then a feature from the last added *)
(*       directory will be used.  *\) *)
(*   val add_directory : string -> (unit,#Context.t) Machine.t *)

(*   (\** [load_features features] loads specified [features] into the *)
(*       Lisp machine. The feature list should be consistent, see below. *)

(*       A feature with a given [name] is provided by a lisp file *)
(*       [name.lisp]. The namespace of features is flat, i.e., it doesn't *)
(*       matter in which directory a file, which provides the feature, is *)
(*       residing. *)

(*       Each lisp module may request more features using the [require] *)
(*       form, so loading one feature may end up in loading a whole set *)
(*       of features. See the [primus_lisp_library] plugin for more *)
(*       information about available features. *)

(*       A lisp module may provide functions with an external linkage, *)
(*       that will be linked with a program in the process of *)
(*       loading. During the linkage all overloaded names will be *)
(*       resolved, and linked. That has two consequences. First, the *)
(*       [features] set should be consistent, i.e., all sets of *)
(*       candidates should be resolvable, otherwise the linkage will *)
(*       fail. Second, if a function is already linked in the previous *)
(*       phase, it will not be linked anymore, aka weak symbol semantics. *)
(*       Basically, these two consequences mean, that it is better to *)
(*       load all features at once. *)
(*   *\) *)
(*   val load_features : string list -> (unit,#Context.t) Machine.t *)
(* end *)


val init : ?paths:string list -> string list -> unit
