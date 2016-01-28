open Bap_types.Std
open Core_kernel.Std

module Move = struct
  type 'a t = {
    cell : 'a;
    data : word;
  } with bin_io, compare, fields, sexp
end

module Chunk = struct
  type t = {
    addr : addr;
    data : string;
  } with bin_io, compare, fields, sexp
end

module Syscall = struct
  type t = {
    number : int;
    args : word array;
  } with bin_io, compare, fields, sexp
end

module Exn = struct
  type t = {
    number : int;
    src : addr option;
    dst : addr option;
  } with bin_io, compare, fields, sexp
end

module Location = struct
  type t = {
    name : string option;
    addr : addr;
  } with bin_io, compare, fields, sexp
end

type location = Location.t with bin_io, compare, sexp

module Call = struct
  type t = {
    caller : location;
    callee : location;
    args : word array;
  } with bin_io, compare, fields, sexp
end

module Return = struct
  type t = {
    caller : string;
    callee : string;
  } with bin_io, compare, fields, sexp
end

module Modload = struct
  type t = {
    name : string;
    low : addr;
    high : addr;
  } with bin_io, compare, fields, sexp
end

type 'a move = 'a Move.t with bin_io, compare, sexp
type chunk = Chunk.t with bin_io, compare, sexp
type syscall = Syscall.t with bin_io, compare, sexp
type exn = Exn.t with bin_io,compare,sexp
type call = Call.t with bin_io,compare,sexp
type return = Return.t with bin_io,compare,sexp
type modload = Modload.t with bin_io,compare,sexp
