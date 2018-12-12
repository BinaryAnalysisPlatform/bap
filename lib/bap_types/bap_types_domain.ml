open Core_kernel
open Bap_knowledge

module Exp = struct
  type t = Bap_bil.exp option [@@deriving compare, sexp_of]
  let inspect = function
    | None -> Sexp.List []
    | Some exp -> Sexp.Atom (Bap_exp.to_string exp)
  let partial x y : Domain.Order.partial = match x,y with
    | None,None -> EQ
    | None,_ -> LE
    | _,None -> GE
    | _ -> NC
  let empty = None
end

module Bil = struct
  type t = Bap_bil.stmt list [@@deriving compare, sexp_of]
  let inspect = function
    | [] -> Sexp.List []
    | bil -> Sexp.Atom (Bap_stmt.Stmts_pp.to_string bil)
  let partial x y : Domain.Order.partial = match x,y with
    | [],[] -> EQ
    | [],_ -> LE
    | _,[] -> GE
    | _ -> NC
  let empty = []
end
let exp = Semantics.declare ~name:"bil-exp" (module Exp)
let bil = Semantics.declare ~name:"bil-stmt" (module Bil)
