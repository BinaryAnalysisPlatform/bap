open Core_kernel

open Caml.Format
open Bap_knowledge
open Bap_core_theory_sort
open Knowledge.Syntax

module Value = Bap_core_theory_value



type ident =
  | Reg of {name : string}
  | Var of {num : int; mut : bool}
[@@deriving bin_io, compare, hash, sexp]

type 'a t = {sort : 'a sort; ident : ident}

let valid_first_char = function
  | 'A'..'Z' | 'a'..'z' | '_' -> true
  | _ -> false

let valid_char c =
  valid_first_char c || match c with
  | '0' .. '9' | '\'' -> true
  | _ -> false

let non_empty name =
  if String.length name = 0
  then invalid_arg "Invalid var literal: a variable can't be empty"

let all_chars_valid name =
  if not (valid_first_char name.[0])
  then invalid_argf
      "Invalid var liter: a variable can't start from %c" name.[0] ();
  match String.find name ~f:(Fn.non valid_char) with
  | None -> ()
  | Some c ->
    invalid_argf
      "Invalid var literal: a variable can't contain char %c" c ()

let validate_variable name =
  non_empty name;
  all_chars_valid name

let define sort name =
  validate_variable name;
  non_empty name;
  {sort; ident = Reg {name}}

let create sort ident = {sort; ident}

let pp_ident ppf ident = match ident with
  | Reg {name} -> Format.fprintf ppf "%s" name
  | Var {num; mut} ->
    Format.fprintf ppf "%s%d" (if mut then "#" else "$") num

let name v = Format.asprintf "%a" pp_ident v.ident
let ident v = v.ident

let sort v = v.sort
let is_virtual v = match v.ident with Var _ -> true | Reg _ -> false
let is_mutable v = match v.ident with Var {mut} -> mut | _ -> true

module Counter() : sig
  val read : int knowledge
  val incr : unit knowledge
  val decr : unit knowledge
end = struct
  let data = Semantics.declare "counter" (module Domain.Chain.Make(struct
        type t = int [@@deriving compare, sexp]
        let empty = 0
        let inspect = sexp_of_t
      end))

  let counter = Knowledge.declare
      ~name:"edu.cmu.ece.bap/fresh-variables"
      ~desc:"fresh variables generator"
      data
  open Knowledge.Syntax
  let read = Knowledge.collect counter Label.root
  let incr =
    Knowledge.collect counter Label.root >>= fun x ->
    Knowledge.provide counter Label.root (succ x)
  let decr =
    Knowledge.collect counter Label.root >>= fun x ->
    Knowledge.provide counter Label.root (pred x)
end

module Generator : sig
  val fresh : 'a sort -> 'a t knowledge
end = struct
  module Counter = Counter()
  let fresh sort =
    Counter.incr >>= fun () ->
    Counter.read >>| fun num ->
    {sort; ident = Var {num; mut=true}}
end

module Scoped : sig
  val create : 'a sort -> ('a t -> 'b Value.t knowledge) -> 'b Value.t knowledge
end = struct
  module Counter = Counter()
  let create sort scope =
    Counter.incr >>= fun () ->
    Counter.read >>= fun num ->
    scope {sort; ident = Var {num; mut=false}} >>= fun x ->
    Counter.decr >>= fun () ->
    !!x
end

let fresh = Generator.fresh
let scoped = Scoped.create

module Id = struct
  type t = ident [@@deriving bin_io, compare, hash, sexp]
  let module_name = "Bap_core_theory.Var.Ident"
  let of_string x =
    let n = String.length x in
    if n = 0
    then invalid_arg "a variable identifier can't be empty";
    Scanf.sscanf x "%c%s" @@ function
    | '#' | '$' as c  -> fun s ->
      let num = try int_of_string s with _ ->
        failwithf "`%s' is not a valid temporary value" x () in
      Var {mut = c = '#'; num}
    | _ -> fun _ ->
      validate_variable x;
      Reg {name=x}

  let to_string x = Format.asprintf "%a" pp_ident x

end
module Ident = Identifiable.Make(Id)
