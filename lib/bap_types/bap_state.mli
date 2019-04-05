open Bap_knowledge

type t

val t : t

val run : 'a Knowledge.cls -> 'a Knowledge.obj Knowledge.t ->
  ('a Knowledge.value, Knowledge.conflict) result

val run_or_fail : 'a Knowledge.cls -> 'a Knowledge.obj Knowledge.t -> 'a Knowledge.value

val reset : unit -> unit
val set : t -> unit

val current : unit -> Knowledge.state
