open Core_kernel
open Bap_core_theory
open Bap.Std

open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute

module Feature = String
module Name = String

type t = Feature.Set.t Name.Map.t [@@deriving compare, equal, sexp]
let empty = Name.Map.empty

type Attribute.error += Unterminated_quote



let fail what got = Attribute.Parse.fail what [got]
let expect_atom = fail Attribute.Parse.Expect_atom
let expect_list = fail Attribute.Parse.Expect_list


let attr proj attr = match Project.get proj attr with
  | Some x -> [x]
  | None -> []

let endian proj = Project.arch proj |>
                  Arch.endian |> function
                  | LittleEndian -> "little"
                  | BigEndian -> "big"
let features = Feature.Set.of_list

let of_project proj =
  let t = Project.target proj in
  let targets =
    t :: Theory.Target.parents t |>
    List.map ~f:Theory.Target.to_string in
  Name.Map.of_alist_exn [
    "arch", features @@ [
      Arch.to_string (Project.arch proj);
    ] @ attr proj Bap_abi.name;
    "abi", features @@ attr proj Bap_abi.name @ [
        Theory.Abi.to_string (Theory.Target.abi t)
      ];
    "fabi", features [
      Theory.Fabi.to_string @@ Theory.Target.fabi t;
    ];
    "filetype", features [
      Theory.Filetype.to_string @@ Theory.Target.filetype t;
    ];
    "endian", features [endian proj];
    "endianness", features [
      Theory.Endianness.to_string @@ Theory.Target.endianness t;
    ];
    "target", features targets;
    "bits", features [sprintf "%d" (Theory.Target.bits t)];
    "byte", features [sprintf "%d" (Theory.Target.byte t)];
    "data-addr-size",
    features [sprintf "%d" (Theory.Target.data_addr_size t)];
    "code-addr-size",
    features [sprintf "%d" (Theory.Target.code_addr_size t)];
    "system", features [
      Theory.System.to_string @@ Theory.Target.system t
    ];


  ]

let create descs =
  List.map descs ~f:(fun (name,xs) -> name,features xs) |>
  Name.Map.of_alist_reduce ~f:Set.union

let sexp_of_context (name,values) =
  Sexp.List (List.map (name :: Set.to_list values)
               ~f:(fun x -> Sexp.Atom x))

let sexp_of (cs : t) =
  Sexp.List (Atom "context" ::
             (Map.to_alist cs |> List.map ~f:sexp_of_context))

let value = function
  | {data=Atom x} -> x
  | s -> expect_atom s

let parse_name = function
  | {data=Atom x} as s ->
    let n = String.length x in
    if n < 2 then x
    else if Char.(x.[0] = '"')
    then if Char.(x.[n-1] = '"')
      then String.sub ~pos:1 ~len:(n-2) x
      else fail Unterminated_quote s
    else x
  | s -> expect_atom s


let context_of_tree = function
  | {data=List (x :: xs)} ->
    parse_name x, Feature.Set.of_list (List.map xs ~f:parse_name)
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

let pp ppf ctxt =
  Sexp.pp_hum ppf (sexp_of ctxt)


(* OLD: [C <= C'] iff for each class c in C, there is a class c' in C'
   such that c >= c', where c >= c' is a superset operation.

   This implies that the set of classes in C is a subset of the set of
   classes in C', since all missing classes can be introduced as
   classes with an empty feature sets. Intuitively, that denotes that
   a definition implicitly states that it is applicable to all
   instances of a missing class.
*)


(* NEW: X <= Y if X is less-specific than Y, i.e., if for each
   class x in X we have a class y in Y so that F(x) is a subset
   of F(y), where F(.) is the set of features.

   When a class is not present in the context we assume that it has an
   empty set of features.

   A definition that has context D is applicable in project that has
   context P if D <= P, i.e., if D is less specific or is as specific
   as P, so that for each feature requested by D we have a matching
   feature in P.
*)

let (<=) xs ys =
  Map.fold_symmetric_diff xs ys
    ~data_equal:String.Set.equal
    ~init:true
    ~f:(fun matches (_,diff) -> matches && match diff with
      | `Left xs -> Set.is_empty xs
      | `Right _ -> true
      | `Unequal (xs,ys) -> Set.is_subset xs ~of_:ys)


let order c1 c2 : KB.Order.partial =
  match c1 <= c2, c2 <= c1 with
  | true, false -> LT
  | true, true  -> EQ
  | false,false -> NC
  | false,true  -> GT

let merge xs ys : t = Map.merge xs ys ~f:(fun ~key:_ -> function
    | `Left v | `Right v -> Some v
    | `Both (x,y) -> Some (Feature.Set.union x y) )

let join xs ys = Ok (merge xs ys)

let domain = KB.Domain.define "context"
    ~empty ~order ~join

let t = Attribute.declare "context"
    ~package:"core"
    ~domain
    ~parse
