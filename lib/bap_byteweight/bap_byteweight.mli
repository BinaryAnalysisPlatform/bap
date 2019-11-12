(** Byteweight library.

    Byteweight is a function start identification algorithm [[1]]. This
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


type stats

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


  (** [next t ~length ~threshold data begin] the next positive chunk.

      Returns an offset that is greater than [begin] of the next
      longest substring up to the given [length], for which
      [h1 / (h0 + h1) > threshold].

      This is a specialization of the [next_if] function from the
      extended [V1.V2.S] interface.
  *)

  val next : t ->
    length:int ->
    threshold:float -> corpus -> int -> int option


  (** [pp ppf decider] prints all known to decider chunks.  *)
  val pp : Format.formatter -> t -> unit
end

module V1 : sig
  module type S = S

  module Make
      (Corpus : Corpus)
      (Trie : Trie.S with type key = Corpus.key) :
    S with type key = Corpus.key
       and type corpus = Corpus.t
end

module V2 : sig
  module type S = sig
    include V1.S
    type token


    (** [next_if t ~length ~f data begin] the next chunk that [f].

        Finds the next offset greater than [begin] of a string of
        the given [length] for which there was an observing of a
        substring [s] with length [n] and statistics [stats], such
        that [f s n stats] is [true].
    *)
    val next_if : t -> length:int -> f:(key -> int -> stats -> bool) -> corpus ->
      int -> int option


    (** [fold t ~init ~f] applies [f] to all chunks known to the decider.   *)
    val fold : t -> init:'b -> f:('b -> token list -> stats -> 'b) -> 'b
  end

  module Make
      (Corpus : Corpus)
      (Trie : Trie.V2.S with type key = Corpus.key) :
    S with type key = Corpus.key
       and type corpus = Corpus.t
       and type token = Trie.token
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
  include V2.S with type key = mem
                and type corpus = mem
                and type token := word


  (** [find mem ~length ~threshold corpus] extract addresses of all
      memory chunks of the specified [length], that were classified
      positively under given [threshold]. *)
  val find : t -> length:int -> threshold:float -> corpus -> addr list


  (** [find_if mem ~length ~f corpus] finds all positively classfied chunks.

      This is a generalization of the [find] function with an arbitrary
      thresholding function.

      It scans the input corpus using the [next_if] function and
      collects all positive results.
  *)
  val find_if : t -> length:int -> f:(key -> int -> stats -> bool) -> corpus -> addr list


  (** [find_using_bayes_factor sigs mem] classify functions starts using the
      Bayes factor procedure.

      Returns a list of addresses in [mem] that have a signature in
      [sigs] with length [min_length <= n <= max_length] and the Bayes
      factor greater than [threshold].

      The Bayes factor is the ratio between posterior probabilities of
      two hypothesis, the [h1] hypothesis that the given sequence of bytes
      occurs at the function start, and the dual [h0] hypothesis,

      [k = P(h1|s)/P(h0|s) = (P(s|h1)/P(s|h0)) * (P(h1)/P(h0))],

      where
      - [P(hN|s)] is the probability of the hypothesis [P(hN)]
        given the sequence of bytes [s] as the evidence,
      - [P(s|hN] is the probability of the sequence of bytes [s],
        given the hypothesis [hN],
      - [P(hN)] is the prior probability of the hypothesis [hN].

      Given that [m] is the total number of occurences of a sequence
      of bytes [s] at the beginning of a function, and [n] is the total
      number of occurences of [s] in a middle of a function, we compute
      [P(s|h1)] and [P(s|h0)] as

      - [P(s|h1) = m / (m+n)],
      - [P(s|h0) = 1 - P(s|h1) = n / (m+n)].

      Given that [q] is the total number of substrings in [sigs] of length
      [min_length <= l <= max_length] and [p] is the total number of
      substrings of the length [l] that start functions, we compute prior
      probabilities as,

      - [P(h1) = p / q],
      - [P(h0) = 1 - P(h1)].


      The resulting factor is a value [0 < k < infinity] that
      quantify the strength of the evidence that a given substring
      gives in support of the hypothesis [h1]. Levels below [1]
      support hypothesis [h0], levels above [1] give some support of
      [h1], with the following interpretations (Kass and Raftery
      (1995)),

      {v
        Bayes Factor          Strength

        1 to 3.2              Weak
        3.2 to 10             Substantial
        10 to 100             Strong
        100 and greater       Decisive
      v}
  *)
  val find_using_bayes_factor : t ->
    min_length:int ->
    max_length:int ->
    float ->
    corpus -> addr list


  (** [find_using_threshold sigs mem] classify function starts using
      a simple thresholding procedure.

      Returns a list of addresses in [mem] that have a signature [s] in
      [sigs] with length [min_length <= n <= max_length] and the
      sample probability [P1(s)] of starting a function greater than
      [threshold],

      [P1(s) = m / (m+n)], where
      - m - the total number of occurences of [s] at the begining
        of a function in [sigs];
      - n - the total number of occurences of [s] not at the begining
        of a function in [sigs].
  *)
  val find_using_threshold : t ->
    min_length:int ->
    max_length:int ->
    float ->
    corpus -> addr list
end


module Stats : sig
  type t = stats


  (** [trial stats] is the total number of trials.

      This is the total number of occurences of the given substring in
      all tests, it is equal to [h0 stats + h1 stats].
  *)
  val trials : t -> int


  (** [h0 stats] is how many times the null-hypothesis being accepted.

      This statistics tells us exactly how many times the label
      function returned false for this substring.

      In terms of the function starts, this is how many times the
      substring was classified as not a function start.
  *)
  val h0 : t -> int


  (** [h1 stats] is how many times the null hypothesis was rejected.

      This statistic tells us exactly how many times the label
      function returned true for this substring.

      In terms of the function starts, this is how many times the
      substring was classified as a function start.
  *)
  val h1 : t -> int
end
