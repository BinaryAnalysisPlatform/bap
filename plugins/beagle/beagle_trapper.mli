open Bap.Std

(** symbol encoding *)
module type Alphabet = sig
  (** total number of symbols in the alphabet *)
  val length : int


  (** [index x] maps [x] to the [n]'th symbol of an alphabet, if [x]
      is a representation of that symbols, returns a number that is
      outside of [[0,len-1]] interval if it is not.*)
  val index : char -> int
end

module Ascii : sig
  module Alpha : sig
    module Caseless : Alphabet
    include Alphabet
  end
  module Alphanum : sig
    module Caseless : Alphabet
    include Alphabet
  end
  module Digits : Alphabet
  module Printable : Alphabet
  include Alphabet
end

module Make(A : Alphabet) : sig
  type t [@@deriving bin_io, compare, sexp]

  val empty : t

  val of_file : string -> t
  val of_files : string list -> t

  val add_word : t -> string -> t
  val build : t -> string -> string seq
  val is_buildable : t -> string -> bool

end
