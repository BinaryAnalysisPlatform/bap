open Core_kernel.Std
open Format
open Bap_trie_intf


module Make(Key : Key) : S with type key = Key.t

module Array : sig
  module Prefix(Tok : Token) : S with type key = Tok.t array
  module Suffix(Tok : Token) : S with type key = Tok.t array
end

module String : sig
  module Prefix : S with type key = string
  module Suffix : S with type key = string
end
