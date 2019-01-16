open Bap_primus_types
module Machine = Bap_primus_machine

type t [@@deriving sexp_of]


val static : word -> t
val random : ?min:word -> ?max:word -> ?seed:word -> int -> t
val custom : min:word -> max:word -> int -> word Machine.t -> t

val next : t -> word Machine.t
