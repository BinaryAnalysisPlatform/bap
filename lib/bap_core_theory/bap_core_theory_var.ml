open Core_kernel

open Caml.Format
open Bap_knowledge
open Bap_core_theory_sort

type 'a t = {
  name : string;
  sort : 'a sort;
  temp : bool;
}

let valid_char = function
  | 'A'..'Z' | 'a'..'z' | '_' | '\''  -> true
  | _ -> false

let non_empty name =
  if String.length name = 0
  then invalid_arg "Invalid var literal: a variable can't be empty"

let all_chars_valid name =
  match String.find name ~f:(Fn.non valid_char) with
  | None -> ()
  | Some c ->
    invalid_argf
      "Invalid var literal: a variable can't contain char %c" c ()

let validate_variable name =
  non_empty name;
  all_chars_valid name

let create sort name =
  validate_variable name;
  {sort; name; temp=false}

let name v = v.name
let sort v = v.sort
let is_virtual v = v.temp

module Generator : sig
  val fresh : 'a sort -> 'a t Knowledge.t
end  = struct
  let data = Semantics.declare "counter"
      (module Domain.Counter)
  let counter = Knowledge.declare
      ~name:"edu.cmu.ece.bap/fresh-variables"
      ~desc:"fresh variables generator"
      data

  open Knowledge.Syntax
  let succ = Domain.Counter.succ

  let fresh sort =
    Knowledge.collect counter Label.root >>= fun x ->
    Knowledge.provide counter Label.root (succ x) >>| fun () ->
    {sort; name = asprintf "$%a" Domain.Counter.pp (succ x); temp=true}
end
