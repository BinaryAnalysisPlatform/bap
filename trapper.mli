open Bap.Std

module type Alphabet = sig
  val length : int
  val index : char -> int
end

module Ascii : sig
  module Alpha : sig
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
