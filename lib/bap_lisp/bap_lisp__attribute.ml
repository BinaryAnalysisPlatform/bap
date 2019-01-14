open Core_kernel.Std
open Bap.Std

open Bap_lisp__types

type attrs = Univ_map.t
type set = attrs

type error = ..

type error += Expect_list

exception Unknown_attr of string * tree
exception Bad_syntax of error * tree list


type 'a attr = {
  key : 'a Univ_map.Key.t;
  add : 'a -> 'a -> 'a;
  parse : tree list -> 'a;
}

type 'a t = 'a Univ_map.Key.t

type parser = Parser of (set -> tree list -> set)

let parsers : parser String.Table.t = String.Table.create ()

let make_parser attr attrs sexp =
  let value = attr.parse sexp in
  Univ_map.update attrs attr.key ~f:(function
      | None -> value
      | Some value' -> attr.add value value')

let register ~name ~add ~parse =
  let attr = {
    key = Univ_map.Key.create ~name sexp_of_opaque;
    add;
    parse = parse;
  } in
  let parser = Parser (make_parser attr) in
  Hashtbl.add_exn parsers ~key:name ~data:parser;
  attr.key

let expected_parsers () =
  String.Table.keys parsers |> String.concat ~sep:" | "

let parse s attrs name values = match Hashtbl.find parsers name with
  | None -> raise (Unknown_attr (name,s))
  | Some (Parser run) -> run attrs values

let parse attrs = function
  | {data=List ({data=Atom name} as s :: values)} -> parse s attrs name values
  | s -> raise (Bad_syntax (Expect_list,[s]))

module Set = struct
  let get = Univ_map.find
  let empty = Univ_map.empty
end
