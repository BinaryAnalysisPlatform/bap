(** C language ABI.

    This module provides a common interface for building ABI support
    modules for C language.
*)

open Core_kernel.Std
open Bap.Std
open Bap_c_type


(** Function formal parameter is represented as a pair of
    an abstraction of data, that is passed via the parameter,
    and a BIL expression, that denotes the parameter.*)
type param = Bap_c_data.t * exp

(** subroutine argument list is split into three parts:
    [return] is the return arguments, that is optional;
    [params] are regular positional parameters, the length
    of the [params] list must equal to the amount of the
    formals in the function prototype;
    [hidden] are hidden parameters, that are inserted by abi
    to pass special arguments, like [this] pointer or a pointer
    to a structural value, for example.

    The api processor, created by this module, will insert arg terms
    into sub in the following way:

    - nth positional argument corresponds to nth arg term (counting
      from 0).
    - the last arg term corresponds to the return argument, if any;
    - all hidden arguments are put between the last positional and the
      return argument.*)
type args = {
  return : param option;
  hidden : (Bap_c_type.t * param) list;
  params : param list;
}

(** an abi processor.
    Each architecture registers its own abi processor, that is
    responsible for dispatching the processed subroutine between
    architecture specific abi processors.*)
type t = {
  (** [insert_args sub attrs proto] infer a list of arguments that
      should be inserted for a subroutine [sub] annotated with the
      attribute list [attrs] *)
  insert_args : sub term -> attr list -> proto -> args option;

  (** [apply_attrs attrs sub] transform a subroutine based on the
      semantics of the list of attributes, attached to it. (Usually,
      this means a transformation of C attributes to BIR
      attributes.)  *)
  apply_attrs : attr list -> sub term -> sub term;
}

(** [create_api_processor arch t] packs processor for the given
    architecture [arch] into an api processor. The api processor will
    insert arg terms into each recognized subroutine, propagate some
    known C attributes into corresponding BIR attributes, annotate
    each inserted arg term with its corresponding C type and datum
    model, and annotate each regognized subroutine with its C
    prototype.

    The api processor relies on an availability of a front end parser
    for C language.*)
val create_api_processor : arch -> t -> Bap_api.t


(** [data size t] creates an abstraction of data that is represented
    by type [t]. The [size] parameter defines a data model, e.g.,
    sizes of primitive types, padding and alignment restrictions, etc.*)
val data : #Bap_c_size.base -> Bap_c_type.t -> Bap_c_data.t

(** [arg_intent t] infers argument intention based on its C type.  If
    an argument is passed by value, i.e., it is a c basic type, then
    it is an input argument. If an argument is a reference, but not a
    function, then it is input/output if any value, referenced by the
    argument is non-const. A reference to function always has the
    input intent. If an argyment is a structure or union, then it is
    input/output if any of its fields is input/output.
*)
val arg_intent : Bap_c_type.t -> intent


(** An abstraction of a stack, commonly used in C compilers.   *)
module Stack : sig
  (** [stack = create ?growsup arch] is a function that returns
      [n]'th stack slot *)
  val create : ?growsup:Bool.t -> arch -> int -> exp
end
