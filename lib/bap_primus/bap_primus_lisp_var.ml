open Core_kernel
open Bap.Std
open Bap_primus_lisp_types

module Type = Bap_primus_lisp_type

let to_string = function
  | {exp;typ = Word} -> exp
  | {exp;typ = Type n} -> sprintf "%s:%d" exp n

let sexp_of_var v = Sexp.Atom (to_string v)

type read_error = Empty | Not_a_var | Bad_type | Bad_format

let read = function
  | "" -> Error Empty
  | x when Char.is_digit x.[0] || x.[0] = '\'' || x.[0] = '"' ->
    Error Not_a_var
  | x -> match String.split x ~on:':' with
    | [] -> assert false
    | _::_::_::_ -> Error Bad_format
    | [x] -> Ok {exp=x; typ=Word}
    | [x;sz] -> match Type.read sz with
      | None -> Error Bad_type
      | Some typ -> Ok {exp=x; typ}


include Comparable.Make_plain(struct
    type t = var [@@deriving compare,sexp_of]
  end)

type t = var [@@deriving compare,sexp_of]
