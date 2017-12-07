open Core_kernel
open Bap_primus_lisp_types

module Type = Bap_primus_lisp_type

type t = word [@@deriving compare]

type read_error = Empty | Not_an_int | Unclosed | Bad_literal | Bad_type

let prime = function '\'' -> true | _ -> false

let char_of_string s =
  try Ok Char.(Int64.of_int @@ to_int @@ of_string s)
  with _ -> Error Bad_literal

let read_char str =
  let char = String.strip ~drop:prime str in
  if String.length str - 2 = String.length char
  then  (char_of_string char)
  else Error Unclosed

let read_int str =
  try Ok (Int64.of_string (String.strip str))
  with _ -> Error Bad_literal

let char id eq s =
  Result.map (read_char s) ~f:(fun exp ->
      {data={exp; typ = Type.word 8}; id; eq})

let int ?typ id eq s =
  Result.bind (read_int s) ~f:(fun exp ->
      match typ with
      | None -> Ok {data={exp;typ=Any}; id; eq}
      | Some s -> match Type.read s with
        | None -> Error Bad_type
        | Some typ -> Ok {data={exp;typ}; id; eq})

let read id eq x =
  if String.is_empty x then Error Empty
  else if x.[0] = '\''
  then char id eq x
  else if Char.is_digit x.[0]
  then match String.split x ~on:':' with
    | [x] -> int id eq x
    | [x;typ] -> int ~typ id eq x
    | _ -> Error Bad_literal
  else Error Not_an_int

let sexp_of_word {data={exp}} = sexp_of_int64 exp
let sexp_of_t = sexp_of_word

include Comparable.Make_plain(struct
    type t = word [@@deriving compare, sexp_of]
  end)
