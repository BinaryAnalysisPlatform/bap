open Core_kernel

open Caml.Format
open Bap_knowledge
open Bap_core_theory_sort
open Knowledge.Syntax

module Value = Knowledge.Value

let package = "core-theory"

type ident =
  | Reg of {name : string; ver : int}
  | Let of {num : Int63.t}
  | Var of {num : Int63.t; ver : int}
[@@deriving bin_io, compare, hash, sexp]

type 'a t = {sort : 'a sort; ident : ident}

let valid_first_char = function
  | 'A'..'Z' | 'a'..'z' | '_' -> true
  | _ -> false

let valid_char c =
  valid_first_char c || match c with
  | '0' .. '9' | '\'' | '.' -> true
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
  {sort; ident = Reg {name; ver=0}}

let create sort ident = {sort; ident}

let pp_ver ppf = function
  | 0 -> ()
  | n -> fprintf ppf ".%d" n

let pp_ident ppf ident = match ident with
  | Reg {name; ver} -> Format.fprintf ppf "%s%a" name pp_ver ver
  | Let {num} ->
    Format.fprintf ppf "$%a" Int63.pp num
  | Var {num; ver} ->
    Format.fprintf ppf "#%a%a" Int63.pp num pp_ver ver

let name v = Format.asprintf "%a" pp_ident v.ident
let ident v = v.ident

let sort v = v.sort
let is_virtual v = match v.ident with
  | Let _ | Var _ -> true
  | Reg _ -> false
let is_mutable v = match v.ident with
  | Let _ -> false
  | Reg _ | Var _ -> true

let nat1 = Knowledge.Domain.total "nat1"
    ~empty:0
    ~inspect:sexp_of_int
    ~order:Int.compare

type const = Const
type mut = Mut

let const = Knowledge.Class.declare ~package "const-var" Const
    ~desc:"local immutable variables"

let mut = Knowledge.Class.declare ~package "mut-var" Mut
    ~desc:"temporary mutable variables"

let versioned s ver = match s.ident with
  | Let _ -> s
  | Reg {name} -> {s with ident = Reg {name; ver}}
  | Var {num} -> {s with ident = Var {num; ver}}

let version s = match s.ident with
  | Let _ -> 0
  | Reg {ver} | Var {ver} -> ver

let fresh s =
  Knowledge.Object.create mut >>| fun v ->
  create s (Var {num = Knowledge.Object.id v; ver=0})

type 'a pure = 'a Sort.exp Knowledge.value knowledge

(* we're ensuring that a variable is immutable by constraining
   the scope computation to be pure. *)
let scoped : 'a sort -> ('a t -> 'b pure) -> 'b pure = fun s f ->
  Knowledge.Object.scoped const @@ fun v ->
  f @@ create s (Var {num = Knowledge.Object.id v; ver=0})

module Ident = struct
  type t = ident [@@deriving bin_io, compare, hash, sexp]

  let num s = try Int63.of_string s with _ ->
    failwithf "`%s' is not a valid temporary value" s ()

  let split_version s =
    match String.rfindi s ~f:(fun _ c -> c = '.') with
    | None -> s,0
    | Some n ->
      String.subo ~len:n s,
      Int.of_string (String.subo ~pos:(n+1) s)

  let of_string x =
    let n = String.length x in
    if n = 0
    then invalid_arg "a variable identifier can't be empty";
    Scanf.sscanf x "%c%s" @@ function
    | '$' -> fun s -> Let {num = num s}
    | '#'  -> fun s ->
      let s,ver = split_version s in
      Var {num = num s; ver}
    | _ -> fun _ ->
      validate_variable x;
      let name,ver = split_version x in
      Reg {name; ver}

  let to_string x = Format.asprintf "%a" pp_ident x
  include Base.Comparable.Make(struct
      type t = ident [@@deriving bin_io, compare, sexp]
    end)

end
type ord = Ident.comparator_witness
