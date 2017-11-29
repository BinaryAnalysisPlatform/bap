open Core_kernel
open Bap_primus_lisp_types

module Context = Bap_primus_lisp_context
module Def = Bap_primus_lisp_def


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
