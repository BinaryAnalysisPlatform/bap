open Core_kernel
open Bap.Std

(** Sexp Parser.

    The parser contains all parsed modules, as well as positional
    information.
*)
module Index = Bap_primus_lisp_index
module Loc = Bap_primus_lisp_loc
module Id : Index.S
module Eq : Index.S

type error
type t
type 'a indexed = ('a,Id.t,Eq.t) Index.interned
type tree = token indexed
and token = Atom of string | List of tree list

val empty : t
val load : t -> string -> (t,error) result
val find : t -> string -> tree list option
val loc : t -> tree -> Loc.t
val filename : t -> tree -> string
val fold : t -> init:'a -> f:(string -> tree list -> 'a -> 'a) -> 'a
