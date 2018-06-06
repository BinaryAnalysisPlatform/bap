open Bap.Std

(** [renum bil] - replaces all virtual variables in [bil]
    with variables v0, v1 ... *)
val renum : bil -> bil
