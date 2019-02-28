open Bap_core_theory_sort

type ('a,'e,'t) t
type ('a,'e,'t) ieee754 = ('a,'e,'t) t

type parameters = private {
  base : int;
  bias : int;
  k : int;
  p : int;
  w : int;
  t : int;
}


val binary16 : parameters
val binary32 : parameters
val binary64 : parameters
val binary80 : parameters
val binary128 : parameters
val decimal32 : parameters
val decimal64 : parameters
val decimal128 : parameters

val binary : int -> parameters option
val decimal : int -> parameters option

module Sort : sig
  open Float
  val define : parameters -> (('b,'e,'t) ieee754,'s) format Float.t sort
  val exps : (('b,'e,'t) ieee754,'s) format Float.t sort -> 'e Bitv.t sort
  val sigs : (('b,'e,'t) ieee754,'s) format Float.t sort -> 't Bitv.t sort
  val bits : (('b,'e,'t) ieee754,'s) format Float.t sort -> 's Bitv.t sort
  val spec : (('b,'e,'t) ieee754,'s) format Float.t sort -> parameters
end
