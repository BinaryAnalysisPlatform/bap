open Core_kernel.Std


(** [copy buf obj pos] is a method to copy object [obj] into a buffer
    [buf], starting from a position [pos] and will return the number
    of bytes written.  XXX: we need the amount of bytes beforehand.
*)
type ('a,'b) copy = 'b -> 'a -> int -> unit
(** *)
type ('a,'b) dump = 'b -> 'a -> unit

type lexbuf  = Lexing.lexbuf
type scanbuf = Scanf.Scanning.scanbuf

module Versioned = struct
  module type S = sig
    type t
    val version : string
  end
end

module type Versioned = Versioned.S
