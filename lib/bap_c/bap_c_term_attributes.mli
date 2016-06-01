(** BIR attributes.  *)
open Bap.Std
open Bap_c_type


(** Abstraction of a data representation of C value. This
    attribute is attached to each inserted arg term, but can be
    further propagated by other passes  *)
val data : Bap_c_data.t tag

(** Function prototype. This attribute is inserted into each
    annotated function. *)
val proto : proto tag

(** A c type associated with a term. This attribute is attached to
    each inserted arg term, but maybe propagated by further by other
    passes. *)
val t : t tag
