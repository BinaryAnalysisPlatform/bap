open Core_kernel

open Caml.Format
open Bap_knowledge
open Bap_core_theory_value
open Knowledge.Syntax

module Value = Knowledge.Value

let package = "core-theory"

type const = Const [@@deriving bin_io, compare, sexp]
type mut = Mut [@@deriving bin_io, compare, sexp]

let const = Knowledge.Class.declare ~package "const-var" Const
    ~desc:"local immutable variables"

let mut = Knowledge.Class.declare ~package "mut-var" Mut
    ~desc:"temporary mutable variables"

type ident =
  | Reg of {name : string; ver : int}
  | Let of {num : Int63.t}
  | Var of {num : Int63.t; ver : int}
[@@deriving bin_io, compare, hash, sexp]

type 'a var = 'a sort * ident
type 'a t = 'a var

let valid_first_char = function
  | '0'..'9' | '#' | '$' -> false
  | _ -> true

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
      "Invalid var literal: a variable can't start from %c" name.[0] ();
  match String.find name ~f:(Fn.non valid_char) with
  | None -> ()
  | Some c ->
    invalid_argf
      "Invalid var literal: a variable can't contain char %c" c ()

let validate_variable name =
  non_empty name;
  all_chars_valid name

let define sort name : 'a var =
  validate_variable name;
  non_empty name;
  sort, Reg {name; ver=0}

let create sort ident = sort,ident

let forget (s,v) = Sort.forget s,v
let resort (_,v) s = s,v

let pp_ver ppf = function
  | 0 -> ()
  | n -> fprintf ppf ".%d" n

let pp_ident ppf ident = match ident with
  | Reg {name; ver} -> Format.fprintf ppf "%s%a" name pp_ver ver
  | Let {num} ->
    Format.fprintf ppf "$%a" Int63.pp num
  | Var {num; ver} ->
    Format.fprintf ppf "#%a%a" Int63.pp num pp_ver ver

let name (_,v) = Format.asprintf "%a" pp_ident v
let ident (_,v) = v
let sort (s,_) = s
let is_virtual v = match ident v with
  | Let _ | Var _ -> true
  | Reg _ -> false
let is_mutable v = match ident v with
  | Let _ -> false
  | Reg _ | Var _ -> true

let nat1 = Knowledge.Domain.total "nat1"
    ~empty:0
    ~inspect:sexp_of_int
    ~order:Int.compare

let versioned (s,v) ver = match v with
  | Let _ -> (s,v)
  | Reg {name} -> s,Reg {name; ver}
  | Var {num} -> s,Var {num; ver}

let version v = match ident v with
  | Let _ -> 0
  | Reg {ver} | Var {ver} -> ver

let fresh s =
  Knowledge.Object.create mut >>| fun v ->
  create s (Var {num = Knowledge.Object.id v; ver=0})

type 'a pure = 'a Bap_core_theory_value.t knowledge

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

module Top : sig
  type t = unit var [@@deriving bin_io, compare, sexp]
  include Base.Comparable.S with type t := t
end = struct
  type t = Sort.Top.t * ident [@@deriving bin_io, sexp]

  include Base.Comparable.Inherit(Ident)(struct
      type t = Sort.Top.t * ident
      let sexp_of_t = sexp_of_t
      let component = snd
    end)
end
