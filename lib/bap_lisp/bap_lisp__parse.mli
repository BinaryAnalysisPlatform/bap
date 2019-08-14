
module Context = Bap_lisp__context
module Program = Bap_lisp__program

type error

val program : ?paths:string list -> Context.t -> string list ->
  (Program.t,error) result

val pp_error : Format.formatter -> error -> unit
val pp_program : Format.formatter -> Program.t -> unit
