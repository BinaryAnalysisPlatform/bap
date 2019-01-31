open Core_kernel
open Bap.Std

module Tracer = struct
  type t = {
    name : string;
    args : string array;
    envp : string array;
    version : string;   (** release or Git hash, or SVN number *)
  } [@@deriving bin_io, compare, sexp]
end

module Binary = struct
  type t = {
    path : string;
    args : string array;
    envp : string array;
    md5sum : string;
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

module Trace_stats = struct
  type t = {
    user : string;   (** Name of a trace creator  *)
    host : string;   (** A host where trace was created *)
    time : float;    (** Time when tracing started  *)
  } [@@deriving bin_io, compare, sexp]
end

type tracer = Tracer.t [@@deriving bin_io, compare, sexp]
type binary = Binary.t [@@deriving bin_io, compare, sexp]
type file_stats = File_stats.t [@@deriving bin_io, compare, sexp]
type trace_stats = Trace_stats.t [@@deriving bin_io, compare, sexp]
