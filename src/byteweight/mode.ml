open Core_kernel.Std
open Bap.Std
type bytes = char list
type prob = float

module type S =
  sig
    type t
    type key = t list
    module Tree : Trie_common.T with type key = key
    module Table : Hashtbl.S with type key = key
    val generate_keys : addr -> ?len:int -> Memory.t -> key list
    val consecutive : ?arch:arch -> bytes -> t list
    val load : string -> unit
    val find : key -> prob
    val len : int
    val string_of_key : key -> string
  end
