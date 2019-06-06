open Bap_knowledge

type 'a t = 'a
type env
type main
type 'p var

val eval : ('a,'p) Knowledge.slot -> 'a Knowledge.obj knowledge -> 'p t
val exec : unit knowledge -> unit t

val try_eval : ('a,'p) Knowledge.slot -> 'a Knowledge.obj knowledge ->
  ('p,Knowledge.conflict) result t

val try_exec : unit knowledge -> (unit,Knowledge.conflict) result t

val get : 'p var -> 'p t
val put : 'p var -> 'p knowledge -> unit t
val var : string -> 'p var


val env : env
val reset : unit -> unit
val set : env -> unit
val current : unit -> Knowledge.state
