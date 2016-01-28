open Core_kernel.Std
open Bap_types.Std

module Tracer = struct
  type t = {
    name : string;
    args : string array;
    version : string;   (** release or Git hash, or SVN number *)
  } with bin_io, compare, sexp
end


module Binary = struct
  type t = {
    path : string;
    stripped : bool option;     (* yes, no, unknown *)
  } with bin_io, compare, sexp
end


module File_stats = struct
  type t = {
    size  : int;
    atime : float;
    mtime : float;
    ctime : float;
  } with bin_io, compare, sexp
end


type tracer = Tracer.t with bin_io, compare, sexp
type binary = Binary.t with bin_io, compare, sexp
type file_stats = File_stats.t with bin_io, compare, sexp
