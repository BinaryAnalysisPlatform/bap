(** Byteweight library.

    Byteweight is a function start identification mechanism [[1]]. This
    library provides a functorized implementation.

    An auxiliary {!Bap_byteweight_signatures} library provides an
    access to the repository of binary signatures.

    @see
    <https://www.usenix.org/system/files/conference/usenixsecurity14/sec14-paper-bao.pdf>
    {v
    [1]: Bao, Tiffany, et al. "Byteweight: Learning to recognize functions in binary code."
         23rd USENIX Security Symposium (USENIX Security 14). 2014.
    v}
  *)

open Bap.Std


(** Data interface.

    This is an interface of a type that is used to represent the
    data.*)
module type Corpus = sig
  type t
  type key

  (** [look data ~length offset] extract data of specified [length] at
      the given [offset]. Returns a key that represents this chunk of
      data (if the data can be extracted).

      Note 1 - for simple data representations, like strings, types
      [t] and [key] can be unified, and the [look] function is just a
      regular substring extraction function.

      Note 2 - the key type is unified with the [key] of {!Trie} data
      structure, so one will need to implement the {!Trie.Key}
      interface.
  *)
  val look : t -> length:int -> int -> key option
end


(** Byteweight algorithm interface.

    Byteweight is a supervised machine learning algorithm. Based on
    the input string, where each substrings is labeled by true of
    false, a function is inferred that can map substrings into boolean
    domain.

    For example, if the label function teaches whether the given
    substring is a start of a function, we can infer an algorithm for
    finding function starts.

*)
module type S = sig
  type t [@@deriving bin_io, sexp]
  type key
  type corpus


  (** [create ()] creates an empty instance of the byteweigth decider.  *)
  val create : unit -> t


  (** [train decider ~max_length test corpus] train the [decider] on
      the specified [corpus]. The [test] function classifies extracted
      substrings.  The [max_length] parameter binds the maximum
      length of substrings. *)
  val train : t -> max_length:int -> (key -> bool) -> corpus -> unit


  (** [length decider] total amount of different substrings known to a
      decider.  *)
  val length : t -> int


  (** [next decider ~length ~threshold data begin] returns an offset
      greater then [begin] of the next substring of the given
      [length], that is positively classified, with the given
      [threshold].  *)
  val next : t ->
    length:int ->
    threshold:float ->
    corpus -> int -> int option


  val pp : Format.formatter -> t -> unit
end


(** [Make(Corpus)(Trie)] creates a Byteweight procedure,
    that works in the [Corpus] domain and uses [Trie] for its
    implementation.

    The [Trie] module specifies how substrings are compared. In
    particular, it gives an opportunity, to implement normalized
    string comparison.*)
module Make
    (Corpus : Corpus)
    (Trie : Trie.S with type key = Corpus.key) :
  S with type key = Corpus.key
     and type corpus = Corpus.t

(** Default implementation that uses memory chunk as the domain.  *)
module Bytes : sig
  include S with type key = mem
             and type corpus = mem

  (** [find mem ~length ~threshold corpus] extract addresses of all
      memory chunks of the specified [length], that were classified
      positively under given [threshold]. *)
  val find : t -> length:int -> threshold:float -> corpus -> addr list
end
