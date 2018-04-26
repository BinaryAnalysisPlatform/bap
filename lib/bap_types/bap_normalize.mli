
open Bap_bil

val normalize_exp : exp -> exp
val bil : ?normalize_exp:bool -> bil -> bil
