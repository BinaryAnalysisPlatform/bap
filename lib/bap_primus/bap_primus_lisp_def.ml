open Core_kernel.Std
open Bap.Std
open Format
open Bap_c.Std
open Bap_primus_types
open Bap_primus_sexp

open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute

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
  subst : sexp;
} [@@deriving fields]

type subst = {
  elts : sexp list;
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
end


module Primitive = struct
  let create ?(docs="") name code : 'a t =
    {meta = {name;docs; attrs=Attribute.Set.empty; loc=Primitive}; code}
end
