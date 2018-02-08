open Core_kernel.Std
open Bap.Std

type t [@@deriving bin_io, compare, sexp]
type rtl = t [@@deriving bin_io, compare, sexp]
type exp [@@deriving bin_io, compare, sexp]

module Exp : sig

  val of_var  : var -> exp
  val of_vars : var list -> exp
  val of_word : word -> exp
  val tmp : int -> exp

  val load : var -> exp -> endian -> size -> exp
  val extract : int -> int -> exp -> exp

  val signed : exp -> exp
  val unsigned : exp -> exp

  val width : exp -> int

  val concat : exp -> exp -> exp

end

val store : var -> exp -> exp -> endian -> size -> t
val if_ : exp -> t list -> t list -> t
val jmp : exp -> t

(** [foreach ~inverse step e code] - repeats [code] for each
    [step] of [e]. if [inverse] is set to true then starts from
    head (most significant bits) of [e] *)
val foreach : inverse:bool -> exp -> exp -> t list -> t

val message : string ->t

module Infix : sig
  val ( := )  : exp -> exp -> rtl
  val ( + )  : exp -> exp -> exp
  val ( - )  : exp -> exp -> exp
  val ( * )  : exp -> exp -> exp
  val ( / )  : exp -> exp -> exp
  val ( ^ )  : exp -> exp -> exp
  val ( % )  : exp -> exp -> exp
  val ( < )  : exp -> exp -> exp
  val ( > )  : exp -> exp -> exp
  val ( <= )  : exp -> exp -> exp
  val ( >= )  : exp -> exp -> exp
  val ( = )  : exp -> exp -> exp
  val ( <> )  : exp -> exp -> exp
  val ( << )  : exp -> exp -> exp
  val ( >> )  : exp -> exp -> exp
  val ( lor )  : exp -> exp -> exp
  val ( land ) : exp -> exp -> exp
  val ( lxor ) : exp -> exp -> exp
  val lnot : exp -> exp
end


(** [bil_of_t d] - returns a program in BIL language   *)
val bil_of_t : t list -> bil
