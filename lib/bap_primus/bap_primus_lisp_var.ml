open Core_kernel
open Bap_core_theory
open Bap.Std
open Bap_primus_lisp_types
open Format

module Type = Bap_primus_lisp_type

let to_string = function
  | {data={exp;typ = Any}} -> KB.Name.to_string exp
  | {data={exp;typ}} -> asprintf "%a:%a" KB.Name.pp exp Type.pp typ

let sexp_of_var v = Sexp.Atom (to_string v)

type read_error = Empty | Not_a_var | Bad_type | Bad_format

let read ?package id eq = function
  | "" -> Error Empty
  | x when Char.is_digit x.[0] || Char.(x.[0] = '\'') || Char.(x.[0] = '"') ->
    Error Not_a_var
  | x -> match String.split x ~on:':' with
    | [] -> assert false
    | _::_::_::_::_ -> Error Bad_format
    | [package;name;sz] -> begin match Type.read sz with
        | None -> Error Bad_type
        | Some typ ->
          let x = KB.Name.create ~package name in
          Ok {data={exp=x; typ}; id; eq}
      end
    | [x] -> Ok {data={exp=KB.Name.read ?package x; typ=Any}; id; eq}
    | [x;sz] -> match Type.read sz with
      | Some typ ->
        Ok {data={exp=KB.Name.read ?package x; typ}; id; eq}
      | None ->
        let x = KB.Name.read ?package (x ^ ":" ^ sz) in
        Ok {data={exp=x; typ=Any}; id; eq}

let reify ~width {data={exp;typ}} =
  let open Bap.Std in
  let exp = KB.Name.show exp in
  match typ with
  | Type t -> Theory.Var.define (Theory.Bitv.define t) exp
  | _ -> Theory.Var.define (Theory.Bitv.define width) exp


include Comparable.Make_plain(struct
    type t = var [@@deriving compare,sexp_of]
  end)

type t = var [@@deriving compare,sexp_of]
