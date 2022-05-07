(* Transforms floating-point operations into bitvector operations.

   The reason why this module is in BIL is because it is not a theory
   but a theory transformer in other words it is not a structure but
   a functor that requires other theory. Right now, the other theory
   is hardcoded to be the BIL theory and using this transformation
   will not affect any other theories. To be able to remove this
   transformation from the BIL plugin we need to finish the Core
   Theory passes framework. Until then, we will keep it here.

   Otherwise, the trasnformation (this module) is totally generic and
   is independent of BIL and could be put into a library if
   necessary.

*)

open Bap_knowledge
open Bap_core_theory

open Theory

type ('b,'e,'t,'s) fsort = (('b,'e,'t) IEEE754.t,'s) format Float.t Value.sort

module Make(_ : Theory.Core) : sig

  val fadd : ('b,'e,'t,'s) fsort -> rmode -> 's bitv ->  's bitv -> 's bitv
  val fsub : ('b,'e,'t,'s) fsort -> rmode -> 's bitv ->  's bitv -> 's bitv
  val fmul : ('b,'e,'t,'s) fsort -> rmode -> 's bitv ->  's bitv -> 's bitv
  val fdiv : ('b,'e,'t,'s) fsort -> rmode -> 's bitv ->  's bitv -> 's bitv
  val fsqrt : ('b,'e,'t,'s) fsort -> rmode -> 's bitv -> 's bitv

  val is_nan : ('b,'e,'t,'s) fsort -> 's bitv -> bool
  val is_finite : ('b,'e,'t,'s) fsort -> 's bitv -> bool
  val is_inf : ('b,'e,'t,'s) fsort -> 's bitv -> bool
  val is_zero : 's bitv -> bool
  val is_fpos : 's bitv -> bool
  val is_fneg : 's bitv -> bool

  val cast_int :  ('a, 'b, 'c, 'd) fsort -> 'e Bitv.t Value.sort -> 'd bitv -> 'e bitv
  val cast_float : ('a, 'b, 'c, 'd) fsort -> rmode -> 'e bitv -> 'd bitv
  val cast_float_signed : ('a, 'b, 'c, 'd) fsort -> rmode -> 'e bitv -> 'd bitv
  val convert : ('b, 'e, 't, 's) fsort -> 's bitv -> rmode ->  ('b, 'a, 'c, 'd) fsort -> 'd bitv
end
