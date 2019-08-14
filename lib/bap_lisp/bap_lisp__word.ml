open Core_kernel
open Bap_lisp__types

module Type = Bap_lisp__type

type t = word [@@deriving compare]

type read_error = Empty | Not_an_int | Unclosed | Bad_literal
                | Bad_type of Type.read_error

let char_of_string s =
  try Ok Char.(Bitvec.M8.int @@ to_int @@ of_string s)
  with _ -> Error Bad_literal

let read_char str = char_of_string (String.subo ~pos:1 str)

let read_int str =
  try Ok (Bitvec.of_string (String.strip str))
  with _ -> Error Bad_literal

let char id eq s =
  Result.map (read_char s) ~f:(fun exp ->
      {data={exp; typ = Type.word 8}; id; eq})

let int ?typ id eq s =
  Result.bind (read_int s) ~f:(fun exp ->
      match typ with
      | None -> Ok {data={exp;typ=Any}; id; eq}
      | Some s -> match Type.read s with
        | Error e -> Error (Bad_type e)
        | Ok typ -> Ok {data={exp;typ}; id; eq})

let base x =
  if String.length x < 3 then 10
  else
    let i = if x.[0] = '-' then 1 else 0 in
    match x.[i], x.[i+1] with
    | '0',('b'|'B') -> 2
    | '0',('o'|'O') -> 8
    | '0',('x'|'X') -> 16
    | _ -> 10

let minimum_bitwidth ~base digits =
  Float.to_int @@
  Float.round_up @@
  float digits *. (log (float base) /. log 2.)

let is_hex = function
  | '0'..'9' | 'a'..'f' | 'A'..'F' -> true
  | _ -> false

let infer_width x =
  let base = base x in
  let sign_bit = if x.[0] = '-' then 1 else 0 in
  let is_digit = if base = 16 then is_hex else Char.is_digit in
  let len = String.count x ~f:is_digit in
  let return n = String.concat [
      x; ":";
      string_of_int (minimum_bitwidth ~base n + sign_bit)
    ] in
  if base = 10 then return len
  else return (len - 1)  (* for the base designator *)

let read id eq x =
  if String.is_empty x then Error Empty
  else if x.[0] = '?'
  then char id eq x
  else if Char.is_digit x.[0] ||
          String.length x > 1 &&
          Char.is_digit x.[1] &&
          x.[0] = '-'
  then match String.split x ~on:':' with
    | [x;typ] -> int ~typ id eq x
    | [x] -> int id eq @@ infer_width x
    | _ -> Error Bad_literal
  else Error Not_an_int

let sexp_of_word {data={exp}} = Sexp.Atom (Bitvec.to_string exp)
let sexp_of_t = sexp_of_word
