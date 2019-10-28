open Core_kernel
open Format
open Bap_trie_intf

module Make(Key : Key) : V2.S with type key = Key.t
                               and type token = Key.token

module Array : sig
  module Prefix(Tok : Token) : V2.S with type key = Tok.t array
                                     and type token = Tok.t
  module Suffix(Tok : Token) : V2.S with type key = Tok.t array
                                     and type token = Tok.t
end

module String : sig
  module Prefix : V2.S with type key = string and type token = char
  module Suffix : V2.S with type key = string and type token = char
end
