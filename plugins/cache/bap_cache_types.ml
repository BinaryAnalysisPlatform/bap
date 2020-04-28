open Core_kernel
open Regular.Std

type config = {
  capacity : int;  (* Mb  *)
  overhead : float;
  gc_enabled : bool;
} [@@deriving bin_io, compare, sexp]

module Compatibility = struct
  module V2 = struct
    type entry = {
      atime   : float;
      ctime   : float;
      hits    : int;
      path    : string;
      size    : int64;
    } [@@deriving bin_io, compare, sexp]

    type config = {
      max_size : int64;
    } [@@deriving bin_io, compare, sexp]

    type t = {
      config  : config;
      entries : entry Data.Cache.Digest.Map.t;
    } [@@deriving bin_io, compare, sexp]
  end
end
