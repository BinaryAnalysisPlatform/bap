(** Extends [exp] interface.  *)
open Core_kernel.Std
open Bap_common
open Bap_bil

include Regular with type t := exp

type binop = exp -> exp -> exp
type unop = exp -> exp

(** Arithmetic operations *)
val ( + )   : binop
val ( - )   : binop
val ( * )   : binop
val ( / )   : binop
val ( /$ )  : binop
val ( mod ) : binop
val ( %$ )  : binop

(** Bit operations *)
val ( lsl ) : binop
val ( lsr ) : binop
val ( asr ) : binop
val ( land) : binop
val ( lor ) : binop
val ( lxor) : binop
val lnot    : unop

(** Equality tests *)
val ( = )   : binop
val ( <> )   : binop
val ( < )   : binop
val ( > )   : binop
val ( <= )   : binop
val ( >= )   : binop
val ( <$ )  : binop
val ( >$ )  : binop
val ( <=$ ) : binop
val ( >=$ ) : binop

(** Misc operations *)
(** [a ^ b] contatenate [a] and [b]  *)
val ( ^ )   : binop
