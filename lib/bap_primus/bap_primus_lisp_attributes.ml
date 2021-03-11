open Core_kernel
open Bap_knowledge
open Bap.Std

open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute
module Var = Bap_primus_lisp_var
module KB = Knowledge

let package = "core"

let fail err t = Attribute.Parse.fail err t

module Variables = struct

  type t = Set.M(Var).t

  let domain = KB.Domain.powerset (module Var) "vars"

  type Attribute.error += Expect_atom
  type Attribute.error += Var_error of Var.read_error

  let var package t = match t with
    | {data=List _} -> fail Expect_atom [t]
    | {data=Atom v; id; eq} -> match Var.read ~package id eq v with
      | Ok v -> v
      | Error err -> fail (Var_error err) [t]

  let parse ~package xs = List.map xs ~f:(var package) |>
                          Var.Set.of_list

  let global = Attribute.declare "global"
      ~package
      ~domain
      ~parse

  let static = Attribute.declare "static"
      ~package
      ~domain
      ~parse
end

type Attribute.error +=
  | Expect_atom
  | Unterminated_quote

let parse_name = function
  | {data=Atom x} as s ->
    let n = String.length x in
    if n < 2 then x
    else if Char.(x.[0] = '"')
    then if Char.(x.[n-1] = '"')
      then String.sub ~pos:1 ~len:(n-2) x
      else fail Unterminated_quote [s]
    else x
  | s -> fail Expect_atom [s]


module External = struct
  type t = String.Set.t

  let domain = KB.Domain.powerset (module String) "names"

  let parse ~package:_ xs = List.map xs ~f:parse_name |>
                            String.Set.of_list

  let t = Attribute.declare "external"
      ~package
      ~domain
      ~parse
end

module Visibility = struct
  type visibility = Private | Public [@@deriving equal]
  type t = visibility list [@@deriving equal]

  type Attribute.error += Expect_public_or_private

  let order x y : KB.Order.partial =
    match List.length x, List.length y with
    | m,n when m < n -> LT
    | m,n when m > n -> GT
    | _ -> if List.equal equal_visibility x y then EQ else NC

  let domain = KB.Domain.define "visibility-list"
      ~empty:[]
      ~order
      ~inspect:(function
          | [] | Public :: _ -> Sexp.Atom "public"
          | _ -> Sexp.Atom "private")
      ~join:(fun was now -> Ok (now@was))

  let parse ~package:_ = function
    | [{data=Atom ":public"}] -> [Public]
    | [{data=Atom ":private"}] -> [Private]
    | s -> fail Expect_public_or_private s

  let t = Attribute.declare "visibility"
      ~package ~domain ~parse

  let is_public = function
    | []  | Public :: _ -> true
    | _ -> false

end


module Advice = struct
  type cmethod = Before | After [@@deriving compare, equal, sexp]

  module Methods = Map.Make_plain(struct
      type t = cmethod [@@deriving compare, equal, sexp]
    end)

  type Attribute.error +=
    | Unknown_method of string
    | Bad_syntax
    | Empty
    | No_targets

  type t = {methods : Set.M(KB.Name).t Methods.t} [@@deriving compare, equal]


  let empty = {methods = Methods.empty}
  let join {methods=xs} {methods=ys} = Ok {
      methods = Map.merge xs ys ~f:(fun ~key:_ -> function
          | `Left xs | `Right xs -> Some xs
          | `Both (xs,ys) -> Some (Set.union xs ys))
    }

  let order xs ys : KB.Order.partial = match compare xs ys with
    | 1 -> GT
    | 0 -> EQ
    | _ -> LT

  let domain = KB.Domain.define "methods"
      ~join ~order ~empty

  let methods = String.Map.of_alist_exn [
      ":before", Before;
      ":after",  After;
    ]

  let targets {methods} m = match Map.find methods m with
    | None -> Set.empty (module KB.Name)
    | Some targets -> targets


  let parse_target package = function
    | {data=Atom x} -> KB.Name.read ~package x
    | s -> fail Expect_atom [s]

  let parse_targets package met ss = match ss with
    | [] -> fail No_targets ss
    | ss ->
      List.fold ss ~init:{methods=Methods.empty} ~f:(fun {methods} t -> {
            methods = Map.update methods met ~f:(function
                | None -> Set.singleton (module KB.Name)
                            (parse_target package t)
                | Some ts -> Set.add ts (parse_target package t))
          })

  let parse ~package trees = match trees with
    | [] -> fail Empty trees
    | {data=List _} as s :: _  ->  fail Bad_syntax [s]
    | {data=Atom s} as lit :: ss ->
      if String.is_empty s then fail (Unknown_method s) [lit];
      match s with
      | ":before" -> parse_targets package Before ss
      | ":after" -> parse_targets package After ss
      | _ when Char.(s.[0] = ':') -> fail (Unknown_method s) [lit]
      | _ -> parse_targets package Before trees

  let t = Attribute.declare "advice"
      ~package:"primus"
      ~domain
      ~parse
end
