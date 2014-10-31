(** Signature defintions for integer types.  *)

(** Basis for a numeric type  *)
module type Base = sig
  type t
  val zero : t
  val one  : t

  val succ : t -> t
  val pred : t -> t
  val abs  : t -> t
  val neg  : t -> t

  val add     : t -> t -> t
  val sub     : t -> t -> t
  val mul     : t -> t -> t
  val div     : t -> t -> t
  val modulo  : t -> t -> t
  val lnot    : t -> t
  val logand  : t -> t -> t
  val logor   : t -> t -> t
  val logxor  : t -> t -> t
  val lshift  : t -> t -> t
  val rshift  : t -> t -> t
  val arshift : t -> t -> t
end

(** A common set of infix operators  *)
module type Infix = sig
  type t
  val ( ~-)  : t -> t
  val ( + )  : t -> t -> t
  val ( - )  : t -> t -> t
  val ( * )  : t -> t -> t
  val ( / )  : t -> t -> t
  val (mod)  : t -> t -> t
  val (land) : t -> t -> t
  val (lor)  : t -> t -> t
  val (lxor) : t -> t -> t
  val (lsl)  : t -> t -> t
  val (lsr)  : t -> t -> t
  val (asr)  : t -> t -> t
end

(** full signature including [Base] and [Infix].

    The module with interface [S] can be derived from any module
    implementing interface [Base] using [Bap_integer.Make] functor.
*)
module type S = sig
  type t
  include Base  with type t := t
  include Infix with type t := t
end
