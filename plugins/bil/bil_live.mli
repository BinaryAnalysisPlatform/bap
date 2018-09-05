open Bap.Std

(** [run bil] removes dead virtual variables and doesn't touch
    physical ones  *)
val run : bil -> bil
