open Core_kernel
open Bap_lisp__types
open Format

module Type = Bap_lisp__type

let to_string = function
  | {data={exp;typ = Any}} -> exp
  | {data={exp;typ}} -> asprintf "%s:%a" exp Type.pp typ

let sexp_of_var v = Sexp.Atom (to_string v)

type read_error = Empty | Not_a_var | Bad_type | Bad_format

let read id eq = function
  | "" -> Error Empty
  | x when Char.is_digit x.[0] || x.[0] = '\'' || x.[0] = '"' ->
    Error Not_a_var
  | x -> match String.split x ~on:':' with
    | [] -> assert false
    | _::_::_::_ -> Error Bad_format
    | [x] -> Ok {data={exp=x; typ=Any}; id; eq}
    | [x;sz] -> match Type.read sz with
      | None -> Error Bad_type
      | Some typ -> Ok {data={exp=x; typ}; id; eq}

include Comparable.Make_plain(struct
    type t = var [@@deriving compare,sexp_of]
  end)

type t = var [@@deriving compare,sexp_of]
