open Core_kernel.Std
open Bap.Std

module Tracer = struct
  type t = {
    name : string;
    args : string array;
    version : string;   (** release or Git hash, or SVN number *)
  } [@@deriving bin_io, compare, sexp]
end


module Binary = struct
  type t = {
    path : string;
    stripped : bool option;     (* yes, no, unknown *)
  } [@@deriving bin_io, compare, sexp]
end


module File_stats = struct
  type t = {
    size  : int;
    atime : float;
    mtime : float;
    ctime : float;
  } [@@deriving bin_io, compare, sexp]
end


type tracer = Tracer.t [@@deriving bin_io, compare, sexp]
type binary = Binary.t [@@deriving bin_io, compare, sexp]
type file_stats = File_stats.t [@@deriving bin_io, compare, sexp]
