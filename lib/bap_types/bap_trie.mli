(** Prefix tries.
    This module provides a functor that will create a Trie data
    structure for a provided key type. The requirements for the
    key are specified by [Key] signature (see below).



*)
open Core_kernel.Std
open Format

(** Key requirements.
    Key is a sequence of tokens of the specified length.
    It is better to use contiguous data structures, like
    arrays as keys, otherwise you can end up with a slow
    implementation (i.e., don't use lists or sequences as
    keys, use strings, bitstrings, arrays, etc). *)
module type Key = sig
  (** the type of key  *)
  type t

  (** type of token must implement bin_prot, be comparable and
      sexpable *)
  type token with bin_io, compare, sexp

  (** [length key] return the amount of tokens in a [key]  *)
  val length : t -> int

  (** [nth_token key n] the [n]'th token of key. Should be O(1) *)
  val nth_token : t -> int -> token

  (** [hash_token tok] efficient hash function for the [token] type.
      If nothing efficient came to mind, just use [Hashtbl.hash]. *)
  val token_hash : token -> int
end

(** Trie interface.

    This is the actual interface of the Trie data structure.
    Trie is a mutable table, that can be seen as a specialized form of
    a hash table.

    Use [Make] functor to create modules, implementing this signature.
    Also look at already predefined modules, like [String] (see
    below), [Bitvector.Trie], [Bil.Trie], [Insn.Trie], etc.
*)
module type S = sig
  (** trie can store arbitrary data  *)
  type 'a t with bin_io, sexp

  (** a key type that is used to lookup data  *)
  type key

  (** [create ()] creates new empty trie  *)
  val create : unit -> 'a t

  (** [add trie ~key ~data] adds [data] associated with [key], if
      [trie] already has some data associated with the [key], then
      it will be overwritten *)
  val add : 'a t -> key:key -> data:'a -> unit

  (** [change trie key f] if trie has [data] associated with [key] then
      [f] will be called with [Some data], otherwise it will be called
      with [None]. If [f] returns [None] then there will be no data
      associated with [key], if [f] returns [Some thing], then [thing]
      will be bound to [key] *)
  val change : 'a t -> key -> ('a option -> 'a option) -> unit

  (** [find trie key] finds data associated with [key]  *)
  val find : 'a t -> key -> 'a option

  (** [remove trie key] removes value bound with [key] if any.  *)
  val remove : 'a t -> key -> unit

  (** [longest_match trie key]   *)
  val longest_match : 'a t -> key -> (int * 'a) option

  val length : 'a t -> int

  val pp : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit
end

module Make(Key : Key) : S with type key = Key.t

module String : S with type key = string
