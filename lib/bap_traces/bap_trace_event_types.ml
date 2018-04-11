open Bap.Std
open Core_kernel

module Move = struct
  type 'a t = {
    cell : 'a;
    data : word;
  } [@@deriving bin_io, compare, fields, sexp]
end

module Chunk = struct
  type t = {
    addr : addr;
    data : string;
  } [@@deriving bin_io, compare, fields, sexp]
end

module Syscall = struct
  type t = {
    number : int;
    args : word array;
  } [@@deriving bin_io, compare, fields, sexp]
end

module Exn = struct
  type t = {
    number : int;
    src : addr option;
    dst : addr option;
  } [@@deriving bin_io, compare, fields, sexp]
end

module Location = struct
  type t = {
    name : string option;
    addr : addr;
  } [@@deriving bin_io, compare, fields, sexp]
end

type location = Location.t [@@deriving bin_io, compare, sexp]

module Call = struct
  type t = {
    caller : location;
    callee : location;
    args : word array;
  } [@@deriving bin_io, compare, fields, sexp]
end

module Return = struct
  type t = {
    caller : string;
    callee : string;
  } [@@deriving bin_io, compare, fields, sexp]
end

module Modload = struct
  type t = {
    name : string;
    low : addr;
    high : addr;
  } [@@deriving bin_io, compare, fields, sexp]
end

type 'a move = 'a Move.t [@@deriving bin_io, compare, sexp]
type chunk = Chunk.t [@@deriving bin_io, compare, sexp]
type syscall = Syscall.t [@@deriving bin_io, compare, sexp]
type exn = Exn.t [@@deriving bin_io, compare, sexp]
type call = Call.t [@@deriving bin_io, compare, sexp]
type return = Return.t [@@deriving bin_io, compare, sexp]
type modload = Modload.t [@@deriving bin_io, compare, sexp]
