open Bap.Std

module Context = Bap_primus_lisp_context
module Program = Bap_primus_lisp_program

type error

val program : ?paths:string list -> Project.t -> string list ->
  (Program.t,error) result

val pp_error : Format.formatter -> error -> unit
