open Bap_knowledge
open Bap_core_theory

open Theory

type ('b,'e,'t,'s) fsort = (('b,'e,'t) IEEE754.t,'s) format Float.t

module Make(B : Theory.Basic) : sig

  val fadd : ('b,'e,'t,'s) fsort -> rmode -> 's bitv ->  's bitv -> 's bitv
  val fsub : ('b,'e,'t,'s) fsort -> rmode -> 's bitv ->  's bitv -> 's bitv
  val fmul : ('b,'e,'t,'s) fsort -> rmode -> 's bitv ->  's bitv -> 's bitv
  val fdiv : ('b,'e,'t,'s) fsort -> rmode -> 's bitv ->  's bitv -> 's bitv
  val fsqrt : ('b,'e,'t,'s) fsort -> rmode -> 's bitv -> 's bitv

  val cast_int :  ('a, 'b, 'c, 'd) fsort -> 'e bitv sort -> 'd bitv -> 'e bitv
  val cast_float : ('a, 'b, 'c, 'd) fsort -> rmode -> 'e bitv -> 'd bitv
  val cast_float_signed : ('a, 'b, 'c, 'd) fsort -> rmode -> 'e bitv -> 'd bitv
  val convert : ('b, 'e, 't, 's) fsort -> 's bitv -> rmode ->  ('b, 'a, 'c, 'd) fsort -> 'd bitv
end
