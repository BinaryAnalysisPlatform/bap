open Core_kernel.Std
open Trie_common

module MakeKey (Unit : Hashtbl.Key) : (K with type key = Unit.t)
module Make (Key : K) : (T with type key = Key.key list)
