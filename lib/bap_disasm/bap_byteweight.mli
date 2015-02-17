open Bap_types.Std

module type Corpus = sig
  type t
  type key
  val look : t -> length:int -> int -> key option
end

module type S = sig
  type t with bin_io, sexp
  type key
  type corpus

  val create : unit -> t
  val train : t -> max_length:int -> (key -> bool) -> corpus -> unit
  val length : t -> int

  val find : t ->
    length:int ->
    threshold:float ->
    corpus -> int -> int option

  val pp : Format.formatter -> t -> unit
end

module Make
    (Corpus : Corpus)
    (Trie : Trie with type key = Corpus.key) :
  S with type key = Corpus.key
     and type corpus = Corpus.t

type mem = Bap_memory.t

module Bytes : S with type key = mem
                  and type corpus = mem
