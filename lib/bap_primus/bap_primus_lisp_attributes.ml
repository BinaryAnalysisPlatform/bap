open Core_kernel
open Bap.Std

open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute
module Var = Bap_primus_lisp_var

let fail err t = raise (Attribute.Bad_syntax (err,t))

module Variables = struct

  type t = var list

  type Attribute.error += Expect_atom
  type Attribute.error += Var_error of Var.read_error

  let var t = match t with
    | {data=List _} -> fail Expect_atom [t]
    | {data=Atom v; id; eq} -> match Var.read id eq v with
      | Ok v -> v
      | Error err -> fail (Var_error err) [t]

  let parse = List.map ~f:var

  let global = Attribute.register
      ~name:"global"
      ~add:List.append
      ~parse

  let static = Attribute.register
      ~name:"static"
      ~add:List.append
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
  type t = string list

  let parse = List.map ~f:parse_name

  let t = Attribute.register
      ~name:"external"
      ~add:List.append
      ~parse
end


module Advice = struct
  type cmethod = Before | After [@@deriving compare, sexp]

  module Methods = Map.Make_plain(struct
      type t = cmethod [@@deriving compare, sexp]
    end)

  type Attribute.error +=
    | Unknown_method of string
    | Bad_syntax
    | Empty
    | No_targets

  type t = {methods : String.Set.t Methods.t}

  let methods = String.Map.of_alist_exn [
      ":before", Before;
      ":after",  After;
    ]

  let targets {methods} m = match Map.find methods m with
    | None -> String.Set.empty
    | Some targets -> targets

  let parse_targets met ss = match ss with
    | [] -> fail No_targets ss
    | ss ->
      List.fold ss ~init:{methods=Methods.empty} ~f:(fun {methods} t -> {
            methods = Map.update methods met ~f:(function
                | None -> String.Set.singleton (parse_name t)
                | Some ts -> Set.add ts (parse_name t))
          })

  let parse trees = match trees with
    | [] -> fail Empty trees
    | {data=List _} as s :: _  ->  fail Bad_syntax [s]
    | {data=Atom s} as lit :: ss ->
      if String.is_empty s then fail (Unknown_method s) [lit];
      match s with
      | ":before" -> parse_targets Before ss
      | ":after" -> parse_targets After ss
      | _ when Char.(s.[0] = ':') -> fail (Unknown_method s) [lit]
      | _ -> parse_targets Before trees

  let add d1 d2 = {
    methods = Map.merge d1.methods d2.methods ~f:(fun ~key -> function
        | `Both (xs,ys) -> Some (Set.union xs ys)
        | `Left xs | `Right xs -> Some xs)
  }

  let t = Attribute.register ~name:"advice"
      ~parse ~add
end
