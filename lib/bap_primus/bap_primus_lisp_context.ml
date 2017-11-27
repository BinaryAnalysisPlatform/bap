open Core_kernel.Std
open Bap.Std

open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute

module Feature = String
module Name = String

type t = Feature.Set.t Name.Map.t
let empty = Name.Map.empty

type Attribute.error += Expect_atom | Expect_list


let fail what got = raise (Attribute.Bad_syntax (what,[got]))
let expect_atom = fail Expect_atom
let expect_list = fail Expect_list


let attr proj attr = match Project.get proj attr with
  | Some x -> [x]
  | None -> []

let endian proj = Project.arch proj |>
                  Arch.endian |> function
                  | LittleEndian -> "little"
                  | BigEndian -> "big"
let features = Feature.Set.of_list

let of_project proj = Name.Map.of_alist_exn [
    "arch", features @@ [
      Arch.to_string (Project.arch proj);
    ] @ attr proj Bap_abi.name;
    "abi", features @@ attr proj Bap_abi.name;
    "endian", features [endian proj]
  ]

let sexp_of_context (name,values) =
  Sexp.List (List.map (name :: Set.to_list values)
               ~f:(fun x -> Sexp.Atom x))

let sexp_of (cs : t) =
  Sexp.List (Atom "context" ::
             (Map.to_alist cs |> List.map ~f:sexp_of_context))

let value = function
  | {data=Atom x} -> x
  | s -> expect_atom s

let context_of_tree = function
  | {data=List ({data=Atom name} :: values)} ->
    name, Feature.Set.of_list (List.map values ~f:value)
  | s -> expect_list s


let push cs name vs =
  Map.update cs name ~f:(function
      | None -> vs
      | Some vs' -> Set.union vs vs')


let parse : tree list -> t =
  List.fold ~init:Name.Map.empty ~f:(fun cs tree ->
      let (name,vs) = context_of_tree tree in
      push cs name vs)

let add cs cs' =
  Map.fold cs ~init:cs' ~f:(fun ~key:name ~data:vs cs' ->
      push cs' name vs)

let t = Attribute.register
    ~name:"context"
    ~add
    ~parse

let pp ppf ctxt =
  Sexp.pp_hum ppf (sexp_of ctxt)


(* [C <= C'] iff for each class c in C, there is a class c' in C'
   such that c >= c', where c >= c' is a superset operation.

   This implies that the set of classes in C is a subset of the set of
   classes in C', since all missing classes can be introduced as
   classes with an empty feature sets. Intuitively, that denotes that
   a definition implicitly states that it is applicable to all
   instances of a missing class.
*)
let (<=) ctxt ctxt' =
  Map.for_alli ctxt ~f:(fun ~key:name ~data:features ->
      match Map.find ctxt' name with
      | None -> false
      | Some features' -> Set.is_subset features' ~of_:features)


type porder = Less | Same | Equiv | More

let compare c1 c2 =
  match c1 <= c2, c2 <= c1 with
  | true, false -> Less
  | true, true  -> Same
  | false,false -> Equiv
  | false,true  -> More
