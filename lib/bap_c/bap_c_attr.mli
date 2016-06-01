(** Attribute processing.

    This module allows to attach a semantic action for C attributes.
    Each action is a term transformation, that should do nothing, if
    an attribute is not known to him.
*)
open Bap.Std
open Bap_c_type

(** a type of action   *)
type 'a pass = attr -> 'a term -> 'a term

(** register an action  *)
val register : sub pass -> unit

(** apply all registered actions *)
val apply : sub pass
