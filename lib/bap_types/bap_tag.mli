(** Extensible variant.

    This module creates an extensible variant type, that resembles
    extensible variant types, introduced in 4.02, but even more safe.

    To extend variant type with a new constructor, use

    [Tag.register constructor_name sexp_of_constructor], where

    constructor name can be any name, and can even clash with previous
    definitions it is guaranteed, that you will receive a new
    representation of the constructor, every time you're calling this
    function even if parameters are the same. The returned value is
    supposed to be exposed in a module, for later use in other
    modules, c.f., [Image] module defines three constructors:
    - [Image.symbol] for Image symbols, that basically can be seen as
      [Image.Symbol of sym]
    - [Image.section] for image sections;
    - [Image.region] for other named image memory regions.

*)
open Core_kernel.Std
open Bap_common

(** Tag constructor of type ['a]  *)
type 'a t   with sexp_of

(** a value injected into extensible variant  *)
type value with sexp_of

(** [register name sexp] creates a new variant constructor, i.e.,
    a new branch in a variant type. This function has no side-effects,
    it just returns a witness of a new constructor, that can be later
    used for storing into [value] and extracting from it. This
    witness should be shared between user and creator of the value *)
val register : string -> ('a -> Sexp.t) -> 'a t

(** [create cons x] creates a value using constructor [cons] and
    argument [x] *)
val create : 'a t -> 'a -> value

(** [value cons] extracts a value associated with a constructor [cons]
    (Essentially, performs a pattern match on the specified variant
    branch) *)
val value : 'a t -> value -> 'a option

(** [is cons v] true if value [v] was constructed with constructor
    [cons] *)
val is  : 'a t -> value -> bool

(** [tagname value] returns a constructor name of the [value]  *)
val tagname : value -> string

(** [name cons] returns a name of a constructor.  *)
val name : 'a t -> string

include Printable with type t := value
