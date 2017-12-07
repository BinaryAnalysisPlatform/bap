open Core_kernel
open Bap.Std
open Bap_primus_lisp_types

module Value = Bap_primus_value
module Context = Bap_primus_lisp_context

type t = typ [@@deriving compare, sexp]
type context = Context.t

val read : string -> typ option
val word : int -> t
val pp : Format.formatter -> t -> unit


(* (\** Signature is a set of all uses and definitions of a function. *)

(*     Signature is irrefutable if for each usage there is a matching *)
(*     definition. *)

(*     Program is gradually typed with a series of refinements each *)
(*     triggered by a newly discovered definition. A definition of a *)
(*     function will refine the function type signature, and if all *)
(*     refinements are consistent and all definitions are irrefutable, *)
(*     then a program is well-typed. *)

(*     A refinement is consistent with the previous refinement if it is a *)
(*     subtype of the previous refinenent. *)
(* *\) *)
(* module Sig : sig *)


(*   type t *)


(*   (\** [top] is a signature that for each usage has type *)
(*       [T x T x .. x T -> T]  *\) *)
(*   val top : t *)


(*   (\** [refine s id cx args t] refines the signature [s] with an *)
(*       application (cx,args,t). *\) *)
(*   val refine : t -> Id.t -> context -> typ list -> typ -> t *)


(*   (\** [define s id cx t] adds a new definition. *\) *)
(*   (\* val define : t -> Id.t -> context -> (typ list -> typ) -> t *\) *)

(*   (\** [refute s] returns a set of applications that refutes the *)
(*       signature. If the set is empty, then the signature is *)
(*       irrefutable.  *\) *)
(*   (\* val refute : t -> Id.Set.t *\) *)


(* end *)

module Check : sig
  val value : t -> Value.t -> bool
  val arg : t -> Arg.t -> bool
end


include Comparable.S_plain with type t := t
