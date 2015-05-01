open Core_kernel.Std
open Format
module type Key = sig
  type t
  type token with bin_io, compare, sexp
  val length : t -> int
  val nth_token : t -> int -> token
  val token_hash : token -> int
end

module type S = sig
  type 'a t with bin_io, sexp
  type key
  val create : unit -> 'a t
  val add : 'a t -> key:key -> data:'a -> unit
  val change : 'a t -> key -> ('a option -> 'a option) -> unit
  val find : 'a t -> key -> 'a option
  val remove : 'a t -> key -> unit
  val longest_match : 'a t -> key -> (int * 'a) option
  val length : 'a t -> int
  val pp : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit
end

module Make(Key : Key) : S with type key = Key.t

module String : S with type key = string
