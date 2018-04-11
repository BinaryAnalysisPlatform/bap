open Core_kernel

(** symbol encoding *)
module type Alphabet = sig
  (** total number of symbols in the alphabet *)
  val length : int


  (** [index x] maps [x] to the [n]'th symbol of an alphabet, if [x]
      is a representation of that symbols, returns a number that is
      outside of [[0,len-1]] interval if it is not.*)
  val index : char -> int
end

(** ASCII Characters

    Also provides, different subsets of the Ascii character set,
    e.g., [Ascii.Digits], [AScii]
*)
module Ascii : sig

  (** Letters *)
  module Alpha : sig

    (** Caseless Letters  *)
    module Caseless : Alphabet
    include Alphabet
  end


  (** Letters and Numbers  *)
  module Alphanum : sig

    (** Caseless Letters *)
    module Caseless : Alphabet
    include Alphabet
  end

  (** Digits *)
  module Digits : Alphabet

  (** All printable ASCII characters *)
  module Printable : Alphabet

  include Alphabet
end

(** [Make(Alphabet)] creates an unscrambler for the given alphabet.

    The unscrambler will build all words from the provided sequence of
    characters, essentially it is the same as playing scrabble. For
    example, from characters [h,e,l,l,o] it will build words "hell",
    "hello", "ell", "hoe", and so on, provided that they are known to
    the unscrambler, i.e., present in its dictionary.

    The unscrambler requires to know the alphabet of the language
    beforehand, because it uses efficient trie-like representation of
    the dictionary that enables O(1) search for words (O(1) in terms
    of the dictionary size).

*)
module Make(A : Alphabet) : sig
  type t [@@deriving bin_io, compare, sexp]

  (** an empty unscrambler that doesn't know any words *)
  val empty : t


  (** [of_file name] reads the dictionary from file [name],
      each word is on a separate line. (the standard linux dictionary
      file format)
  *)
  val of_file : string -> t

  (** [of_files names] reads the dictionary from all provided file
      [names], each file should be a sequence of newline separated
      words.*)
  val of_files : string list -> t


  (** [add_word d x] extends the dictionary [d] with the new word [x]. *)
  val add_word : t -> string -> t


  (** [build d chars] returns a sequence of all words in the
      dictionary [d] that could be built from the sequence of characters [chars]  *)
  val build : t -> string -> string Sequence.t

  (** [is_buildable d chars] returns [true] if in the dictionary [d]
      exists a word that can be built from the given characters. *)
  val is_buildable : t -> string -> bool
end
