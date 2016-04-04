open Core_kernel.Std
open Bap.Std

type arg_size =
  | Word                        (** same size as CPU word  *)
  | Size of Size.t              (** the specified size     *)
  [@@deriving bin_io, compare, sexp]

exception Attr_type   of string * string [@@deriving sexp]
exception Attr_index  of int * int       [@@deriving sexp]
exception Attr_arity  of string          [@@deriving sexp]


type pos =
  | Ret_0
  | Ret_1
  | Arg of int
  [@@deriving bin_io, compare, sexp]

type attr = {
  attr_name : string;
  attr_args : string list;
} [@@deriving bin_io, compare, sexp]


type arg = {
  arg_name : string;
  arg_pos  : pos;
  arg_intent : intent;
  arg_size : arg_size;
} [@@deriving bin_io, compare, sexp]

type t = {
  args : arg list;
  attrs : attr list;
} [@@deriving bin_io, compare, sexp]
