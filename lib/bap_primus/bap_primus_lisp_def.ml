open Core_kernel
open Bap.Std
open Format
open Bap_c.Std
open Bap_primus_types
open Bap_primus_sexp

open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute
module Loc = Bap_primus_lisp_loc
module Index = Bap_primus_lisp_index
module Type = Bap_primus_lisp_type


type attrs = Attribute.set

type meta = {
  name : string;
  docs : string;
  attrs : attrs;
} [@@deriving fields]

type func = {
  args : var list;
  body : ast;
} [@@deriving fields]

type meth = func

type macro = {
  param : string list;
  subst : tree;
} [@@deriving fields]

type subst = {
  elts : tree list;
} [@@deriving fields]


type const = {
  value : string;
}

type para = {
  default : ast;
}

type 'a primitive = (value list -> 'a)


module type Closure = functor(Machine : Machine) -> sig
  val run : value list -> value Machine.t
end

type closure = (module Closure)

type prim = {
  lambda : closure;
  types : Type.signature option;
}

type 'a spec = {meta : meta; code : 'a}
type 'a t = 'a spec indexed
type 'a def = ?docs:string -> ?attrs:attrs -> string -> 'a

let name {data={meta}} = name meta
let docs {data={meta}} = docs meta
let attributes {data={meta}} = attrs meta

let field f t = f t.data.code

let create data tree = {
  data;
  id = tree.id;
  eq = tree.eq;
}

module Func = struct
  let args = field args
  let body = field body
  let create ?(docs="") ?(attrs=Attribute.Set.empty) name args body =
    create {
      meta = {name; docs; attrs};
      code = {args; body}
    }

  let with_body t body = {
    t with data = {
      t.data with code = {t.data.code with body}}
  }

end

module Meth = Func

module Para =  struct
  let create : 'a def =
    fun ?(docs="") ?(attrs=Attribute.Set.empty) name default ->
      create {
        meta = {name; docs; attrs};
        code = {default};
      }

  let default p = p.data.code.default
  let with_default t default = {
    t with data = {
      t.data with code = {default}
    }
  }
end

module Macro = struct
  type error += Bad_subst of tree * tree list
  let args = field param
  let body = field subst
  let create ?(docs="") ?(attrs=Attribute.Set.empty) name param subst =
    create {
      meta = {name; docs; attrs};
      code = {param; subst}
    }

  let take_rest xs ys =
    let rec take xs ys zs = match xs,ys with
      | [],[] -> Some zs
      | [x], (_ :: _ :: _ as rest) -> Some ((x,rest)::zs)
      | x :: xs, y :: ys -> take xs ys ((x,[y])::zs)
      | _ :: _, [] | [],_ -> None in
    match take xs ys []with
    | Some [] -> Some (0,[])
    | Some ((_,rest) :: _ as bs) ->
      Some (List.length rest, List.rev bs)
    | None -> None

  let bind macro cs = take_rest macro.data.code.param cs

  let find = List.Assoc.find ~equal:String.equal

  let unknown = Eq.null

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

  let apply macro cs = subst cs macro.data.code.subst
end

module Const = struct
  let create ?(docs="") ?(attrs=Attribute.Set.empty) name ~value =
    create {
      meta = {name; docs; attrs};
      code = {value}
    }
  let value p = {data=Atom p.data.code.value; id=p.id; eq=p.eq}
end

module Subst = struct
  type syntax = Ident | Ascii | Hex

  let body = field elts


  let create ?(docs="") ?(attrs=Attribute.Set.empty) name elts =
    create {
      meta = {name; docs; attrs};
      code = {elts}
    }

end


module Primitive = struct
  type nonrec 'a t = 'a primitive t
  let create ?(docs="") name code = {
    data = {
      meta = {name;docs; attrs=Attribute.Set.empty};
      code;
    };
    id = Id.null;
    eq = Eq.null;
  }

  let body p = p.data.code
end


module type Primitives = functor (Machine : Machine) ->  sig
  val defs : unit -> value Machine.t Primitive.t list
end

type primitives = (module Primitives)

module Closure = struct
  let of_primitive prim lambda =
    {prim with data={prim.data with code={
         lambda;
         types=None;
       }}}

  let create ?types ?(docs="") name lambda = {
    data = {
      meta = {name;docs; attrs=Attribute.Set.empty};
      code = {
        types;
        lambda;
      };
    };
    id = Id.null;
    eq = Eq.null;
  }
  let body p = p.data.code.lambda

  let signature p = p.data.code.types
end
