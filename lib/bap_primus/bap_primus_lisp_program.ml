open Core_kernel
open Bap_primus_lisp_types
open Format

module Context = Bap_primus_lisp_context
module Def = Bap_primus_lisp_def
module Var = Bap_primus_lisp_var

type t = {
  context : Context.t;
  sources : Source.t;
  codes : Def.closure Def.t list;
  macros : Def.macro Def.t list;
  substs : Def.subst Def.t list;
  consts : Def.const Def.t list;
  defs : Def.func Def.t list;
} [@@deriving fields]

let empty = {
  context = Context.empty;
  sources = Source.empty;
  codes = [];
  defs = [];
  macros=[];
  substs=[];
  consts=[];
}

type 'a item = ([`Read | `Set_and_create ], t, 'a Def.t list) Fieldslib.Field.t_with_perm

module Items = struct
  let macro = Fields.macros
  let subst = Fields.substs
  let const = Fields.consts
  let func = Fields.defs
  let primitive = Fields.codes
end

let add p (fld : 'a item) x =
  Field.fset fld p (x :: Field.get fld p)

let get p (fld : 'a item) = Field.get fld p

let with_context p context = {p with context}
let with_sources p sources = {p with sources}

let pp_term pp_exp ppf = function
  | {exp; typ=Word} -> fprintf ppf "%a" pp_exp exp
  | {exp; typ=Type n} -> fprintf ppf "%a:%d" pp_exp exp n

let pp_word = pp_term Int64.pp
let pp_var = pp_term String.pp


let rec concat_prog =
  List.concat_map ~f:(function
      | {data=Seq xs} -> concat_prog xs
      | x -> [x])

module Ast = struct
  let rec pp ppf {data} = pp_exp ppf data
  and pp_exp ppf = function
    | Int x ->
      pp_word ppf x
    | Var x ->
      pp_var ppf x
    | Ite (c,t,e) ->
      fprintf ppf "@[<2>(if@ %a@;<1 2>%a@ %a)@]" pp c pp t pp_prog e
    | Let (v,e,b) ->
      fprintf ppf "@[(let@;<1 2>@[<2>(%a@ %a)@]@ %a)@]" pp_var v pp e pp b
    | App (b,xs) ->
      fprintf ppf "@[<2>(%a@ %a)@]" pp_binding b pp_exps xs;
    | Seq [] -> fprintf ppf "()"
    | Seq [x] -> pp ppf x
    | Seq xs ->
      fprintf ppf "@[<2>(prog@ @[<v>%a@])@]" pp_exps (concat_prog xs)
    | Set (v,x) ->
      fprintf ppf "@[<2>(set@ %a@ %a)@]" pp_var v pp x
    | Rep (c,b) ->
      fprintf ppf "@[<2>(while@;<1 2>%a@ @[<v>%a@])@]" pp c pp_prog b
    | Msg (f,es) ->
      fprintf ppf "@[<2>(msg@ \"%a\"@ %a)@]" pp_fmt f pp_exps es;
    | Err msg ->
      fprintf ppf "@[<2>(error@ %s)@]" msg
  and pp_binding ppf = function
    | Dynamic x -> fprintf ppf "%s" x
    | Static _ -> fprintf ppf "<lambda>"
  and pp_exps ppf xs = pp_print_list ~pp_sep:pp_print_space pp ppf xs
  and pp_fmt ppf xs = pp_print_list pp_fmt_elt ppf xs
  and pp_fmt_elt ppf = function
    | Lit s -> pp_print_string ppf s
    | Pos n -> fprintf ppf "$%d" n
  and pp_prog ppf = function
    | {data=Seq xs} -> pp_exps ppf (concat_prog xs)
    | exp -> pp ppf exp
end

let pp_def ppf d =
  fprintf ppf "@[<2>(defun %s @[<2>(%a)@]@ %a)@]@,"
    (Def.name d)
    (pp_print_list ~pp_sep:pp_print_space pp_var) (Def.Func.args d)
    Ast.pp_prog (Def.Func.body d)

let pp ppf {defs} =
  fprintf ppf "@[<v>%a@]" (pp_print_list pp_def) defs
