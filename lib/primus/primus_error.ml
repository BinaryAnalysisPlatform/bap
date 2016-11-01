type 'v state = {
  visited: string list;
  unvisited: string list;
  value: 'v;}

type ('a,+'b) parser =
  | Parser of ('a state -> 'b state list)

type 'a result =
  | Ok of 'a
  | Err

let parseString x = Ok x
let parseInt x = Ok (int_of_string x)
let custom = fun stringToSomething  ->
  Parser (fun { visited; unvisited; value }  ->
      match unvisited with
      | [] -> []
      | next::rest ->
        (match stringToSomething next with
         | Ok (nextValue) ->
           [{
             visited = (next :: visited);
             unvisited = rest;
             value = (value nextValue)
           }]
         | Err  -> []))

let stringp =
  let prev = ref None in
  let reps = ref 0 in
  custom parseString

let intp = custom parseInt

open Core_kernel.Std

type t = exn = ..
let to_string err = Caml.Printexc.to_string err
let add_printer pr = Caml.Printexc.register_printer pr
