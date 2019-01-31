open Bap_knowledge
open Bap_core_theory

type 'a t = 'a knowledge

type ('b, 'e, 't, 's) fsort = (('b,'e,'t) IEEE754.t,'s) format float sort

type ('b, 'e, 't, 's) unop =
  ( ('b, 'e, 't, 's) fsort -> rmode value t -> 's bitv value t -> 's bitv value t)

type ('b, 'e, 't, 's) binop =
  ( ('b, 'e, 't, 's) fsort ->
    rmode value t -> 's bitv value t ->  's bitv value t -> 's bitv value t)


module Make(B : Theory.Basic) : sig

  val rne : rmode value t
  val rna : rmode value t
  val rtp : rmode value t
  val rtn : rmode value t
  val rtz : rmode value t

  val fadd : ('b, 'e, 't, 's) binop
  val fsub : ('b, 'e, 't, 's) binop
  val fmul : ('b, 'e, 't, 's) binop
  val fdiv : ('b, 'e, 't, 's) binop
  val fsqrt : ('b, 'e, 't, 's) unop

  val cast_int :  ('a, 'b, 'c, 'd) fsort -> 'e bitv sort -> 'd bitv value t -> 'e bitv value t
  val cast_float : ('a, 'b, 'c, 'd) fsort -> rmode value t -> 'e bitv value t -> 'd bitv value t
  val cast_float_signed : ('a, 'b, 'c, 'd) fsort -> rmode value t -> 'e bitv value t -> 'd bitv value t

  val convert : ('b, 'e, 't, 's) fsort -> 's bitv value t -> rmode value t ->
                ('b, 'a, 'c, 'd) fsort -> 'd bitv value t


end
