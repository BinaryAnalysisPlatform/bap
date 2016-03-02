open Core_kernel.Std
open Bap_types.Std

module Location = struct
  type t = {
    addr : addr;
    len  : int;
  } [@@deriving bin_io, compare, fields, sexp]
end

type location = Location.t [@@deriving bin_io, compare, sexp]

module type Memory_iterators = sig
  type t
  type 'a m
  val fold     : ?word_size:size -> t -> init:'b -> f:(word -> 'b -> 'b m) -> 'b m
  val iter     : ?word_size:size -> t -> f:(word -> unit m) -> unit m
  val foldi    : ?word_size:size -> t -> init:'b -> f:(addr -> word -> 'b -> 'b m) -> 'b m
  val iteri    : ?word_size:size -> t -> f:(addr -> word -> unit m) -> unit m
  val exists   : ?word_size:size -> t -> f:(addr -> word -> bool m) -> bool m
  val for_all  : ?word_size:size -> t -> f:(addr -> word -> bool m) -> bool m
  val count    : ?word_size:size -> t -> f:(addr -> word -> bool m) -> int m
  val find_if  : ?word_size:size -> t -> f:(addr -> word -> bool m) -> word option m
  val find_map : ?word_size:size -> t -> f:(addr -> word -> 'a option m) ->
    'a option m
end
