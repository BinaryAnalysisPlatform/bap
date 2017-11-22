open Core_kernel.Std
open Bap.Std
open Format
open Bap_c.Std
open Bap_primus_types
open Bap_primus_sexp

open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute
module Loc = Bap_primus_lisp_loc

type attrs = Attribute.set

type meta = {
  name : string;
  docs : string;
  attrs : attrs;
  loc : loc;
} [@@deriving fields]

type func = {
  args : var list;
  body : exp;
} [@@deriving fields]

type macro = {
  param : string list;
  subst : tree;
} [@@deriving fields]

type subst = {
  elts : tree list;
} [@@deriving fields]


type const = macro

type 'a primitive = (value list -> 'a)
type 'a t = {meta : meta; code : 'a}
type 'a def = ?docs:string -> ?attrs:attrs -> loc -> string -> 'a

let attributes = attrs
let location = loc

let field f t = f t.code

module Func = struct
  let args = field args
  let body = field body
  let create ?(docs="") ?(attrs=Attribute.Set.empty) loc name args body = {
    meta = {name; docs; attrs; loc};
    code = {args; body}
  }
end

module Macro = struct
  type error += Bad_subst of tree * tree list
  let args = field param
  let body = field subst
  let create ?(docs="") ?(attrs=Attribute.Set.empty) loc name param subst = {
    meta = {name; docs; attrs; loc};
    code = {param; subst}
  }

  let take_rest xs ys =
    let rec take xs ys zs = match xs,ys with
      | [],[] -> Some zs
      | [x], (_ :: _ :: ys as rest) -> Some ((x,rest)::zs)
      | x :: xs, y :: ys -> take xs ys ((x,[y])::zs)
      | _ :: _, [] | [],_ -> None in
    match take xs ys []with
    | Some [] -> Some (0,[])
    | Some ((z,rest) :: _ as bs) ->
      Some (List.length rest, List.rev bs)
    | None -> None

  let bind macro cs = take_rest macro.code.param cs

  let find = List.Assoc.find ~equal:String.equal

  let unknown = Parser.Eq.null

  let subst bs body =
    let rec sub : tree -> tree list = function
      | {data=List xs; id} ->
        [{data=List (List.concat_map xs ~f:sub); id; eq=unknown}]
      | {data=Atom x} as atom -> match find bs x with
        | None -> [atom]
        | Some cs -> cs in
    match body with
    | {data=List xs; id} ->
      {data=List (List.concat_map xs ~f:sub); id; eq=unknown}
    | {data=Atom x} as atom -> match find bs x with
      | None -> atom
      | Some [x] -> x
      | Some xs -> raise (Fail (Bad_subst (atom,xs)))


  let apply macro cur cs = subst cs macro.code.subst
end

module Const = struct
  let create ?(docs="") ?(attrs=Attribute.Set.empty) loc name subst = {
    meta = {name; docs; attrs; loc};
    code = {param=[]; subst}
  }
end

module Subst = struct
  let create ?(docs="") ?(attrs=Attribute.Set.empty) loc name elts = {
    meta = {name; docs; attrs; loc};
    code = {elts}
  }
  let body = field elts

  let ascii xs =
    let rec loop xs acc = match xs with
      | [] -> acc
      | Atom x :: xs ->
        String.fold x ~init:acc ~f:(fun acc x -> x :: acc) |>
        loop xs
      | List _ :: _ ->
        failwith "ascii syntax must contain only atoms" in
    List.rev_map (loop xs []) ~f:(fun c ->
        Atom (sprintf "%#02x" (Char.to_int c)))

  let hex xs =
    let rec loop xs acc = match xs with
      | [] -> List.rev acc
      | List _ :: _ -> failwith "hex-data must contain only atoms"
      | Atom x :: xs ->
        let x = if is_odd (String.length x) then "0" ^ x else x in
        String.foldi x ~init:acc ~f:(fun i acc _ ->
            if is_odd i
            then (Atom (sprintf "0x%c%c" x.[i-1] x.[i])) :: acc
            else acc) |>
        loop xs in
    loop xs []

end


module Primitive = struct
  let create ?(docs="") name code : 'a t =
    {meta = {name;docs; attrs=Attribute.Set.empty; loc=Primitive};
     code}


end
