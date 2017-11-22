open Core_kernel.Std
open Bap.Std

module Attribute = Bap_primus_lisp_attribute
module Parse = Bap_primus_lisp_parse
module Var = Bap_primus_lisp_var

module Variables = struct

  type t = var list

  let var = function
    | Sexp.Atom v -> Parse.variable v
    | s -> Parse.error Bad_syntax s

  let of_sexp = List.map ~f:var
  let sexp_of vs = Sexp.List (List.map vs ~f:Var.sexp_of_t)

  let global = Attribute.register
      ~name:"global"
      ~add:List.append
      ~sexp_of ~of_sexp

  let static = Attribute.register
      ~name:"static"
      ~add:List.append
      ~sexp_of ~of_sexp
end

module External = struct
  type t = string list

  type Parse.error += Bad_syntax

  let sexp_of x = Sexp.List (List.map x ~f:(fun s -> Sexp.Atom s))
  let of_sexp = List.map ~f:(function
      | Sexp.Atom x -> x
      | s -> Parse.error Bad_syntax s)

  let t = Attribute.register
      ~name:"external"
      ~add:List.append
      ~sexp_of ~of_sexp

end


module Advice = struct
  type cmethod = Before | After

  type Parse.error += Unknown_method of string | Bad_syntax | Empty

  type t = {
    cmethod : cmethod;
    targets : String.Set.t;
  }

  let methods = String.Map.of_alist_exn [
    ":before", Before;
    ":after",  After;
  ]

  let targets _ _ = assert false

  let start = function
    | [] -> invalid_arg "shit"
    | Sexp.List _ as s :: _  -> Parse.error Bad_syntax s
    | Atom s as lit :: _ as ss ->
      if String.is_empty s then Parse.error Empty lit;
      if s.[0] <> ':' then targets Before ss
      else match Map.find methods s with
        | None -> Parse.error (Unknown_method s) lit
        | Some m -> targets m ss




end
