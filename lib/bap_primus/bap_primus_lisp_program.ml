open Bap.Std
open Bap_core_theory
open Core_kernel
open Graphlib.Std
open Regular.Std
open Monads.Std
open Bap_primus_lisp_types
open Format


module Lisp = struct
  module Context = Bap_primus_lisp_context
  module Type = Bap_primus_lisp_type
  module Attribute = Bap_primus_lisp_attribute
end

module Def = Bap_primus_lisp_def

type package = {
  codes : Def.prim Def.t list;
  macros : Def.macro Def.t list;
  substs : Def.subst Def.t list;
  consts : Def.const Def.t list;
  defs : Def.func Def.t list;
  mets : Def.meth Def.t list;
  pars : Def.para Def.t list;
  sigs : Def.signal Def.t list;
} [@@deriving fields]

type t = {
  context : Lisp.Context.t;
  package : string;
  sources : Source.t;
  imports : Set.M(String).t Map.M(String).t;
  library : package Map.M(String).t;
} [@@deriving fields]

type program = t

let default_package = "user"

let empty_package = {
  codes = [];
  defs = [];
  mets = [];
  pars = [];
  sigs = [];
  macros=[];
  substs=[];
  consts=[];
}

let empty = {
  context = Lisp.Context.empty;
  sources = Source.empty;
  package = default_package;
  imports = Map.empty (module String);
  library = Map.empty (module String);
}

let with_package program package = {program with package}
let package program = program.package
let reset_package program =
  with_package program default_package

let get_package program package =
  match Map.find program.library package with
  | None -> empty_package
  | Some pkg -> pkg


let fold_library library ~init ~f =
  Map.fold library ~init ~f:(fun ~key:package ~data init ->
      f ~package data init)

let (++) xs ys : _ Def.t list =
  let add (ids,defs) def =
    if Set.mem ids def.id then ids,defs
    else Set.add ids def.id, def::defs in
  let init = Set.empty (module Id),[] in
  snd@@List.fold ys ~f:add ~init:(List.fold ~init xs ~f:add)

let merge_packages p1 p2 = {
  codes = p1.codes ++ p2.codes;
  defs = p1.defs ++ p2.defs;
  mets = p1.mets ++ p2.mets;
  pars = p1.pars ++ p2.pars;
  sigs = p1.sigs ++ p2.sigs;
  macros = p1.macros ++ p2.macros;
  substs = p1.substs ++ p2.substs;
  consts = p1.consts ++ p2.consts;
}

let use_package program ?(target=program.package) from = {
  program with
  imports = Map.update program.imports target ~f:(function
      | None -> Set.singleton (module String) from;
      | Some imports -> Set.add imports from);
  library = Map.update program.library target ~f:(function
      | None -> get_package program from
      | Some pkg -> merge_packages pkg (get_package program from))
}

let equal p1 p2 =
  Source.equal p1.sources p2.sources


let is_empty p = Map.is_empty p.library

let merge_libraries l1 l2 =
  Map.merge_skewed l1 l2 ~combine:(fun ~key:_ -> merge_packages)

let merge p1 p2 =
  let p1,p2 = if is_empty p1 then
      p1,p2 else
      p2,p1 in {
    p2 with
    context = Lisp.Context.merge p1.context p2.context;
    imports = Map.merge_skewed p1.imports p2.imports
        ~combine:(fun ~key:_ -> Set.union)
  }


type full_access = [`Read | `Set_and_create ]
type 'a item =
  (full_access, package, 'a Def.t list) Fieldslib.Field.t_with_perm

module Items = struct
  open Fields_of_package
  let macro = macros
  let subst = substs
  let const = consts
  let func = defs
  let meth = mets
  let para = pars
  let primitive = codes
  let signal = sigs
end

let add_to_package (fld : 'a item) x p =
  Field.fset fld p (x :: Field.get fld p)

let add prog fld elt =
  let name = KB.Name.read ~package:prog.package (Def.name elt) in
  let package = KB.Name.package name in
  let name = KB.Name.unqualified name in
  let elt = Def.rename elt name in
  let packages = match Map.find prog.imports package with
    | None -> Set.singleton (module String) package
    | Some packages -> Set.add packages package in {
    prog with
    library = Set.fold packages ~f:(fun lib pkg ->
        Map.update lib pkg ~f:(function
            | None -> add_to_package fld elt empty_package
            | Some pkg -> add_to_package fld elt pkg))
        ~init:prog.library;
  }


let get p (fld : 'a item) =
  match Map.find p.library p.package with
  | None -> []
  | Some pkg -> Field.get fld pkg

let fold {library} fld ~init ~f =
  Map.fold library ~init ~f:(fun ~key:package ~data x ->
      f ~package (Field.get fld data) x)

let in_package package p f = f {p with package}



let with_context p context = {p with context}
let with_sources p sources = {p with sources}

let (++) = Map.merge ~f:(fun ~key:_ -> function
    | `Both (id,_) | `Left id | `Right id -> Some id)

let union init xs ~f =
  List.fold xs ~init ~f:(fun vs x -> vs ++ f x)

type node =
  | Entry
  | Defun of Id.t
  | Exit
[@@deriving compare]

module Callgraph = struct
  module G = Graphlib.Make(struct
      type t = node
      include Opaque.Make(struct
          type t = node [@@deriving compare]
          let hash = Hashtbl.hash
        end)
    end)(Unit)

  let empty = Set.empty (module KB.Name)
  let (++) = Set.union
  let call = Set.singleton (module KB.Name)
  let union xs ~f =
    List.map xs ~f |>
    Set.union_list (module KB.Name)

  let rec calls = function
    | {data=App ((Dynamic v),xs); } -> call v ++ union xs ~f:calls
    | {data=(Var _ | Int _ | Sym _ | Err _)} -> empty
    | {data=Ite (x,y,z)} -> calls x ++ calls y ++ calls z
    | {data=(Seq xs | App (_,xs) | Msg (_,xs))} -> union xs ~f:calls
    | {data=(Let (_,x,y) | Rep (x,y))} -> calls x ++ calls y
    | {data=Set (_,x)} -> calls x

  (** computes a mapping from name to id of definitions  *)
  let compute_ids defs =
    let init = Map.empty (module KB.Name) in
    fold_library defs ~init ~f:(fun ~package {defs} init ->
        List.fold defs ~init ~f:(fun ids def ->
            let name = KB.Name.create ~package (Def.name def) in
            Map.add_multi ids ~key:name ~data:def.id))


  let edge id id' =
    G.Edge.create (Defun id) (Defun id') ()

  let build_kernel defs =
    let ids =
      let ids = compute_ids defs in
      fun name -> match Map.find ids name with
        | None -> []
        | Some x -> x in
    Map.fold ~init:G.empty defs ~f:(fun ~key:_ ~data:{defs} g ->
        List.fold defs ~init:g ~f:(fun g def ->
            let g = G.Node.insert (Defun def.id) g in
            Set.fold (calls (Def.Func.body def)) ~init:g ~f:(fun g name ->
                List.fold (ids name) ~init:g ~f:(fun g id ->
                    G.Edge.insert (edge def.id id) g))))

  let connect_with_entry = function
    | Entry -> ident
    | n ->
      G.Edge.insert @@
      G.Edge.create Entry n ()

  let connect_with_exit = function
    | Exit -> ident
    | n ->
      G.Edge.insert @@
      G.Edge.create n Exit ()

  let close dir g =
    let fix = match dir with
      | `In -> connect_with_entry
      | `Out -> connect_with_exit in
    G.nodes g |> Seq.fold ~init:g ~f:(fun g n ->
        if G.Node.degree ~dir n g = 0
        then fix n g
        else g)

  let build library =
    close `Out (close `In (build_kernel library)) |> fun g ->
    Graphlib.depth_first_search (module G) g
      ~init:g ~start:Entry
      ~start_tree:connect_with_entry |> fun g ->
    Graphlib.depth_first_search (module G) g
      ~init:g ~start:Exit ~rev:true
      ~start_tree:connect_with_exit
  include G
end

let pp_callgraph ppf g =
  Graphlib.to_dot (module Callgraph)
    ~formatter:ppf
    ~string_of_node:(function
        | Entry -> "<entry>"
        | Exit -> "<exit>"
        | Defun id -> asprintf "%a" Id.pp id)
    g

let pp_term pp_exp ppf = function
  | {data={exp; typ=Any}} ->
    fprintf ppf "@[<v2>%a@]" pp_exp exp
  | {data={exp; typ}} ->
    fprintf ppf "@[<v2>%a:%a@]" pp_exp exp Lisp.Type.pp typ
let pp_word = pp_term Z.pp_print
let pp_var = pp_term KB.Name.pp

let rec concat_prog p =
  List.concat_map p ~f:(function
      | {data=Seq xs} -> concat_prog xs
      | x -> [x])

module Ast = struct
  let rec pp ppf {data} =
    fprintf ppf "@[<v2>%a@]" pp_exp data
  and pp_exp ppf = function
    | Int x ->
      pp_word ppf x
    | Sym x ->
      KB.Name.pp ppf x.data
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
    | Dynamic x -> fprintf ppf "%a" KB.Name.pp x
    | Static _ -> fprintf ppf "<lambda>"
  and pp_exps ppf xs = pp_print_list ~pp_sep:pp_print_space pp ppf xs
  and pp_fmt ppf xs = pp_print_list pp_fmt_elt ppf xs
  and pp_fmt_elt ppf = function
    | Lit s -> pp_print_string ppf s
    | Pos n -> fprintf ppf "$%d" n
  and pp_prog ppf = function
    | {data=Seq xs} ->
      fprintf ppf "%a" pp_exps (concat_prog xs)
    | exp -> pp ppf exp
end

let pp_def ppf d =
  fprintf ppf "@[<2>(defun %s @[<2>(%a)@]@ %a)@]@,"
    (Def.name d)
    (pp_print_list ~pp_sep:pp_print_space pp_var) (Def.Func.args d)
    Ast.pp_prog (Def.Func.body d)

let pp_met ppf d =
  fprintf ppf "@[<2>(defmethod %s @[<2>(%a)@]@ %a)@]@,"
    (Def.name d)
    (pp_print_list ~pp_sep:pp_print_space pp_var) (Def.Meth.args d)
    Ast.pp_prog (Def.Meth.body d)

let pp_par ppf d =
  fprintf ppf "@[<2>(defparamerter %s@,%a@,%S)@]"
    (Def.name d)
    Ast.pp_prog (Def.Para.default d)
    (Def.docs d)

let pp_package ppf {pars; mets; defs;} =
  let pp_items pp items =
    fprintf ppf "@[<v>%a@]" (pp_print_list pp) items in
  pp_items pp_par pars;
  pp_items pp_met mets;
  pp_items pp_def defs

let pp ppf prog =
  Map.iteri prog.library ~f:(fun ~key:package ~data:pkg ->
      Format.fprintf ppf "(in-package %s)@\n%a@\n" package
        pp_package pkg)

let pp_ast ppf ast = Ast.pp ppf ast

module Use = struct
  let empty = Map.empty (module KB.Name)
  let union = union empty
  let use = Map.singleton (module KB.Name)

  type t = {
    calls : Id.t Map.M(KB.Name).t Id.Map.t;
    vars  : Id.t Map.M(KB.Name).t Id.Map.t; (* def -> var -> use  *)
  }

  let vars bound ast =
    let use bound {exp=v} id =
      if Set.mem bound v then empty
      else use v id in
    let rec free bound = function
      | {data=(Int _ | Err _ | Sym _)}  -> empty
      | {data=Var v; id} -> use bound v.data id
      | {data=Ite (x,y,z)} -> free bound x ++ free bound y ++ free bound z
      | {data=Let (v,x,y)} -> free bound x ++ free (Set.add bound v.data.exp) y
      | {data=(Seq xs | App (_,xs) | Msg (_,xs))}  -> union xs ~f:(free bound)
      | {data=Set (v,x); id} -> use bound v.data id ++ free bound x
      | {data=Rep (x,y)} -> free bound x ++ free bound y in
    free bound ast

  let rec calls = function
    | {data=App ((Dynamic v),_); id} -> use v id
    | {data=(Var _ | Int _ | Sym _ | Err _)} -> empty
    | {data=Ite (x,y,z)} -> calls x ++ calls y ++ calls z
    | {data=(Seq xs | App (_,xs) | Msg (_,xs))} -> union xs ~f:calls
    | {data=(Let (_,x,y) | Rep (x,y))} -> calls x ++ calls y
    | {data=Set (_,x)} -> calls x

  let collect {defs} =
    let init = {calls = Id.Map.empty; vars = Id.Map.empty} in
    List.fold ~init defs ~f:(fun s def ->
        let body = Def.Func.body def in
        let bound = Def.Func.args def |>
                    List.map ~f:(fun {data={exp}} -> exp) |>
                    Set.of_list (module KB.Name) in
        let vs = vars bound body in
        let cs = calls body in
        {
          calls = Map.set s.calls ~key:def.id ~data:cs;
          vars = Map.set s.vars ~key:def.id ~data:vs;
        })
end

(** Assign fresh indices to trees that were produced my macros or that
 ** have no indices at all.
 **
 ** We first scan through all meta definitions (i.e., macros, substs,
 ** and consts) to obtain a set of indices that we shall rewrite, and
 ** then perform rewriting for all program definitions (defs, mets,
 ** and pars)
 **
 ** The newly generated Ids are derived (i.e., associated) with their
 ** base ids, so that if needed their origin can be always
 ** established. (except if their origin was the null identifier).
 **
 ** Motivation: since we identify an ast by its identifier, we want
 ** the trees produced by the term rewriting to have different
 ** identifiers. Otherwise, they could be unified, for example in the
 ** Type checker.
 **)
module Reindex : sig
  val program : program -> program
end = struct
  module State = Monad.State.Make(Source)(Monad.Ident)
  open State.Syntax
  type 'a m = 'a Monad.State.T1(Source)(Monad.Ident).t

  let rec ids_of_trees trees =
    List.fold trees ~init:Id.Set.empty ~f:(fun xs t -> match t with
        | {data=Atom _; id} -> Set.add xs id
        | {data=List ts;id} ->
          Set.union (Set.add xs id) (ids_of_trees ts))

  let ids_of_defs defs map reduce =
    Id.Set.union_list [
      Id.Set.of_list @@ List.map defs ~f:(fun d -> d.id);
      map defs ~f:reduce |> ids_of_trees
    ]

  let macro_ids p = Id.Set.union_list [
      ids_of_defs p.macros List.map Def.Macro.body;
      ids_of_defs p.consts List.map Def.Const.value;
      ids_of_defs p.substs List.concat_map Def.Subst.body;
    ]

  let derive from  =
    State.get () >>= fun src ->
    let nextid = Id.next (Source.lastid src) in
    State.put (Source.derived src ~from nextid) >>| fun () ->
    nextid

  let reindex (get,set) macros def =
    let rename t =
      if Set.mem macros t.id || Id.(null = t.id)
      then derive t.id >>| fun id -> {t with id}
      else State.return t in
    let rec map : ast -> ast m = fun t ->
      rename t >>= fun t -> match t.data with
      | Err _ -> State.return t
      | Int x ->
        rename x >>| fun x ->
        {t with data = Int x}
      | Sym s ->
        rename s >>| fun s ->
        {t with data = Sym s}
      | Var v ->
        rename v >>| fun v ->
        {t with data = Var v}
      | Ite (x,y,z) ->
        map x >>= fun x ->
        map y >>= fun y ->
        map z >>| fun z ->
        {t with data = Ite (x,y,z)}
      | Let (c,x,y) ->
        rename c >>= fun c ->
        map x >>= fun x ->
        map y >>| fun y ->
        {t with data = Let (c,x,y)}
      | Rep (x,y) ->
        map x >>= fun x ->
        map y >>| fun y ->
        {t with data = Rep (x,y)}
      | App (b,xs) ->
        map_all xs >>| fun xs ->
        {t with data = App (b,xs)}
      | Msg (f,xs) ->
        map_all xs >>| fun xs ->
        {t with data = Msg (f,xs)}
      | Seq xs ->
        map_all xs >>| fun xs ->
        {t with data = Seq xs}
      | Set (v,x) ->
        rename v >>= fun v ->
        map x >>| fun x ->
        {t with data = Set (v,x)}
    and map_all xs = State.List.map xs ~f:map in
    map (get def) >>| set def

  let reindex_package p =
    let def = Def.Func.body,Def.Func.with_body in
    let met = Def.Meth.body,Def.Meth.with_body in
    let par = Def.Para.default,Def.Para.with_default in
    let macros = macro_ids p in
    State.List.map p.defs ~f:(reindex def macros) >>= fun defs ->
    State.List.map p.mets ~f:(reindex met macros) >>= fun mets ->
    State.List.map p.pars ~f:(reindex par macros) >>= fun pars ->
    State.return (defs,mets,pars)

  let package prog pkgname =
    let pkg = get_package prog pkgname in
    let (defs,mets,pars),sources =
      State.run (reindex_package pkg) prog.sources in
    {
      prog with
      sources;
      library = Map.set prog.library pkgname {
          pkg with
          defs; mets; pars
        }
    }

  let program prog =
    Map.keys prog.library |>
    List.fold ~init:prog ~f:package

end

module Typing = struct
  (* An expression in Primus Lisp gradual type system may have several
     types, e.g., [(if c 123 'hello)] is a well-typed expression that
     has type int+sym. The int+sym type is a disjunctive type,
     or a polytype. Our type system is _soft_ as we have type Any,
     that denotes a disjunction (join in our parlance) of all
     types. The set of type expressions forms a lattice with the Any
     type representing the Top element (all possible types). The Bot
     type is an empty disjunction.

     Our type inference system is a mixture of flow based and
     inference based analysis. We infer a type of a function,
     and find a fixed point solution for a set of (possibly
     mutually recursive) functions. The inferred type is the upper
     approximation of a program behavior, i.e., it doesn't guarantee
     an absence of runtime errors, though it guarantees some
     consistency of the program static properties.
  *)

  (* Type value (aka type). A program value could be either a symbol
     or a bitvector with the given width. All types have the same
     runtime representation (modulo bitwidth).  *)
  type tval =
    | Tany
    | Tsym
    | Grnd of int
  [@@deriving compare, sexp_of]

  type signature = Lisp.Type.signature = {
    args : typ list;
    rest : typ option;
    ret  : typ;
  } [@@deriving compare, sexp]

  type type_error =
    | Unresolved_variable of Id.t * string
    | Unresolved_function of Id.t * string * signature list
    | Unresolved_signal of Id.t * string * signature option
    | Unresolved_parameter of Id.t * string
    | No_unification of Id.t * tval * Id.t * tval
  [@@deriving compare, sexp_of]

  module Tvals : sig
    type t [@@deriving compare, sexp_of]
    val create : Id.t -> tval -> t
    val fail : Id.t -> type_error -> t
    val meet : Id.t -> t -> t -> t
    val result : t -> (tval,type_error) result
  end = struct
    module Set = Set.Make_plain(struct
        type t = tval [@@deriving compare, sexp_of]
      end)


    type t = {
      result : (tval,type_error) Result.t;
      origin : Id.t;
    } [@@deriving compare, sexp_of]

    let fail where what = {
      result = Error what;
      origin = where;
    }

    let create origin t = {
      result = Ok t;
      origin;
    }

    let is_any = function {result=Ok Tany} -> true | _ -> false
    let is_bot x = Result.is_error x.result

    let meet_tvals x y t1 t2 = match t1,t2 with
      | Tany,_| _,Tany -> Ok Tany
      | _ -> if compare_tval t1 t2 = 0 then Ok t1
        else Error (No_unification (x,t1,y,t2))

    let meet id x y = match x.result,y.result with
      | Error _,_ -> x
      | _,Error _ -> y
      | Ok t1,Ok t2 -> {
          result = meet_tvals x.origin y.origin t1 t2;
          origin = id;
        }

    let result x = x.result
  end

  (* type variables
     we allow users to specify type variables, the rest of the type
     variables are created by using term identifiers of corresponding
     program terms (thus we don't need to create fresh type variables).*)
  type tvar =
    | Name of string
    | Tvar of Id.t
  [@@deriving compare, sexp_of]

  module Tvar = Comparable.Make_plain(struct
      type t = tvar [@@deriving compare, sexp_of]
    end)

  (** Typing environment.

      Typing constraint is built as a composition of rules, where each
      rule is a function of type [gamma -> gamma], so the rules can be
      composed with the function composition operator.
  *)
  module Gamma : sig
    type t [@@deriving compare, sexp_of]

    type rule = t -> t
    type group

    val empty : t


    (** [fail x] marks [x] as an unsound expression, which violates
        an external [rule].  *)
    val fail : Id.t -> type_error -> rule

    (** [get gamma exp] returns a type of the expression [exp].
        If [None] is returned, then the expression doesn't have any
        statical constraints, so its type is [Any]. If some set is
        returned, then this set denotes a disjunction of types, that a
        term can have during program evaluation. If this set is empty,
        then the term is ill-typed.
    *)
    val get : t -> Id.t -> Tvals.t option

    (** [exps gamma] returns a list of typed expressions. *)
    val exps : t -> Id.t list

    (** [constr exp typ] expression [exp] shall have type [typ]  *)
    val constr : Id.t -> typ -> rule

    val widen : Id.t -> rule

    (** [meet x y] types of [x] and [y] shall have the same type,
        which is the greatest lower bound of types [x] and [y]. *)
    val unify : Id.t -> Id.t -> rule

    (** [meets x xs] meets all types in [x::xs]  *)
    val unify_all : Id.t -> Id.t list -> rule

    (** [merge xs ys]  *)
    val merge : t -> t -> t

    (** [partition g] returns a partition of the set of expressions.

        Each class denotes a set of expressions that represent the
        same set of values. If the set of values of that group is
        empty, then those expressions are ill-typed.
    *)
    val partition : t -> group list

    module Group : sig
      (** the set of expressions that belong to the group. *)
      val elements : group -> Id.Set.t

      (** [values groups] is the set of values that all elements of
          the equivalence class share. [None] denotes a set of all
          sets, i.e., absence of any constraints for the expressions
          in the group.
      *)
      val values : group -> Tvals.t option
    end


    val pp : formatter -> t -> unit

  end = struct
    (* typing environment.

       [vars] associates each program term with a type variable. It is a
       disjoint set that partitions the set of program terms into
       equivalence classes, such that two terms belonging to the same
       set will have the same type variable.

       [vals] is the typing environment that associates each type
       variable with the sum of type values (ground types). If an type
       variable is not mapped in [vals] then it is assumed to has type
       Top (i.e., it is a set of all possible types). An empty set
       denotes the bottom type, i.e., all expressions that has that type
       are ill-typed.

       Notation:
       - x,y,z - terms (represented by their identifiers);
       - u,v,w - type variables (aka term classes);
       - r,s,t - type values.

       - var[x] - type variable for x, aka class of x;
       - val[v] - a value of type variable v;
       - var[x] <- v - is a mapping where var[x] is v;
       - val[v] <- t - is a mapping where val[v] is t;
       - g - is a pair of val and var mappings.

       - var[x] is a total mapping represented with a finite mapping
         vars[x] s.t., var[x] is  vars[x] if x in vars, otherwise [Tvar x].
       - val[u] is a total mapping represented with a finite mapping
         vals[u] s.t., val[u] is vals[u] if u in vals, otherwise T,
         where T is the set of all sets excluding T itself.
    *)
    type t = {
      vars : tvar Id.Map.t;
      vals : Tvals.t Tvar.Map.t;
    } [@@deriving compare, sexp_of]

    type rule = t -> t

    type group = {
      var : tvar;
      values : Tvals.t option;
      elements : Id.Set.t;
    }

    let empty = {
      vars = Id.Map.empty;
      vals = Tvar.Map.empty;
    }

    let get g x = match Map.find g.vars x with
      | None -> None
      | Some v -> Map.find g.vals v

    let add_var x v g =
      {g with vars = Map.add_exn g.vars x v}

    let set_var x v g =
      {g with vars = Map.set g.vars x v}

    let set_val v t g =
      {g with vals = Map.set g.vals ~key:v ~data:t}

    let del_val v g =
      {g with vals = Map.remove g.vals v}

    let pp_tvar ppf = function
      | Name s -> fprintf ppf "%s" s
      | Tvar n -> fprintf ppf "t/%a" Id.pp n

    (* val[min u v] <- val[u] /\ val[v] *)
    let meet_types x u v g =
      let w = Tvar.min u v in
      match Map.find g.vals u, Map.find g.vals v with
      | None, None -> g
      | None, Some t | Some t, None -> set_val w t g
      | Some t, Some t' -> set_val w (Tvals.meet x t t') g


    let unify_types x u v g =
      let u,v = if Tvar.(u < v) then u,v else v,u in
      let init = del_val v (meet_types x u v g) in
      Map.fold g.vars ~init ~f:(fun ~key:z ~data:w g ->
          if Tvar.equal v w then set_var z u g else g)

    (* if x <> y then
          let w = min var[x] var[y]
          1. val[w] <- val[var[x]] /\ val[var[y]]
          2. var[x] <- w
          3. var[y] <- w
          4. forall z s.t. var[z] = w: var[z] <- w
          5. max var[x] var[y] is not in vals
    *)
    let unify x y g =
      if Id.equal x y then g
      else match Map.find g.vars x, Map.find g.vars y with
        | None,Some t -> add_var x t g
        | Some t,None -> add_var y t g
        | None,None ->
          let t = Tvar (Id.min x y) in
          add_var x t @@
          add_var y t g
        | Some u,Some v when Tvar.equal u v -> g
        | Some u,Some v -> unify_types (Id.min x y) u v g

    let unify_all x xs g =
      List.fold xs ~init:g ~f:(fun g x' -> unify x x' g)

    let transfer x u g1 g =
      let g = add_var x u g in
      {g with vals = match Map.find g1.vals u with
           | None -> g.vals
           | Some t -> Map.set g.vals u t}

    (* merges g2 into g1.

       For all terms x in typing environment g1,
       if class of x is in g2 is greater than class of x in g1,
       then set type of x in g1 to a meet of type x in g1 and g2.

    *)
    let copy g1 g2 g =
      Map.fold g1.vars ~init:g ~f:(fun ~key:x ~data:u g ->
          match Map.find g2.vars x with
          | None -> g
          | Some v ->
            if Tvar.(u < v)
            then
              let g = add_var x u g in
              match Map.find g1.vals u, Map.find g2.vals v with
              | None,None -> g
              | Some t,None | None, Some t -> set_val u t g
              | Some s, Some t -> set_val u (Tvals.meet x s t) g
            else g)


    let merge g1 g2 =
      (* first we merge parts that do not conflict *)
      let g = {
        vars = Map.merge g1.vars g2.vars ~f:(fun ~key:_ -> function
            | `Left u | `Right u -> Some u
            | `Both (u,v) when Tvar.equal u v -> Some u
            | `Both _ -> None);
        vals = Map.merge g1.vals g2.vals ~f:(fun ~key:_ -> function
            | `Left t | `Right t -> Some t
            | `Both (u,v) when Tvals.compare u v = 0 -> Some u
            | `Both _ -> None);
      } in
      (* then resolve the intersections *)
      let g = copy g1 g2 g in
      let g = copy g2 g1 g in
      g

    let with_tvar x g f = match Map.find g.vars x with
      | None -> f (Tvar x) (add_var x (Tvar x) g)
      | Some u -> f u g

    let fail x err g = with_tvar x g @@ fun u g ->
      set_val u (Tvals.fail x err) g


    let constr_name x n g = with_tvar x g @@ fun u g ->
      unify_types x u (Name n) g

    let constr_grnd x t g = with_tvar x g @@ fun u g ->
      let t = Tvals.create x t in
      match Map.find g.vals u with
      | None -> set_val u t g
      | Some t' -> set_val u (Tvals.meet x t t') g

    let widen x g = constr_grnd x Tany g

    let constr id t g = match t with
      | Any -> g
      | Name n -> constr_name id n g
      | Symbol -> constr_grnd id Tsym g
      | Type n -> constr_grnd id (Grnd n) g

    let join_vars vars =
      let init = Tvar.Map.empty in
      Map.fold vars ~init ~f:(fun ~key:x ~data:u joined ->
          Map.add_multi joined u x)

    let partition {vars; vals} =
      join_vars vars |>
      Map.fold ~init:[] ~f:(fun ~key ~data xs -> {
            var = key;
            elements=Id.Set.of_list data;
            values = Map.find vals key;
          } :: xs)

    module Group = struct
      let elements g = g.elements
      let values g = g.values
    end

    let pp_exps =
      pp_print_list ~pp_sep:(fun ppf () ->
          fprintf ppf ", ")
        Id.pp

    let pp_val ppf = function
      | Tany -> fprintf ppf "any"
      | Tsym -> fprintf ppf "sym"
      | Grnd n -> fprintf ppf "%d" n

    let pp_vals_list =
      pp_print_list ~pp_sep:(fun ppf () ->
          fprintf ppf "+")
        pp_val

    let pp_vals ppf = function
      | None -> fprintf ppf "T"
      | Some t -> match Tvals.result t with
        | Ok t -> pp_val ppf t
        | Error (No_unification (_,t1,_,t2)) ->
          fprintf ppf "%a <> %a" pp_val t1 pp_val t2
        | Error (Unresolved_variable (_,v)) ->
          fprintf ppf "ill-typed(unresolve-variable %s)" v
        | Error (Unresolved_function (_,s,_)) ->
          fprintf ppf "ill-typed(unresolve-function %s)" s
        | Error (Unresolved_signal (_,s,_)) ->
          fprintf ppf "ill-typed(unresolved-signal %s)" s
        | Error (Unresolved_parameter (_,s)) ->
          fprintf ppf "ill-typed(unresolved-parameter %s)" s


    let pp ppf {vars; vals} =
      Map.iteri (join_vars vars) ~f:(fun ~key:u ~data:xs ->
          fprintf ppf "(%a)[%a] : %a@\n"
            pp_exps xs pp_tvar u pp_vals
            (Map.find vals u))

    let exps {vars} = Map.keys vars

  end

  type t = {
    ctxt : Lisp.Context.t;
    globs : int Map.M(KB.Name).t;
    prims : signature Map.M(KB.Name).t;
    funcs : Def.func Def.t list Map.M(KB.Name).t;
    paras : Set.M(KB.Name).t;
  }

  let pp_args ppf args =
    pp_print_list Lisp.Type.pp ppf args
      ~pp_sep:(fun ppf () -> pp_print_char ppf ' ')

  let pp_signature ppf {args; rest; ret} =
    fprintf ppf "@[(%a" pp_args args;
    Option.iter rest ~f:(fun rest ->
        fprintf ppf "&rest %a" Lisp.Type.pp rest);
    fprintf ppf ")@] => (%a)" Lisp.Type.pp ret

  let pp_tval ppf = function
    | Tsym -> fprintf ppf "sym"
    | Grnd n -> fprintf ppf "%d" n
    | Tany -> fprintf ppf "any"

  let pp_plus ppf () = pp_print_char ppf '+'
  let pp_tvals ppf tvals =
    if Set.is_empty tvals
    then fprintf ppf "nil"
    else fprintf ppf "%a"
        (pp_print_list ~pp_sep:pp_plus pp_tval)
        (Set.elements tvals)

  let apply_return appid ret g =
    match ret with
    | Any -> Gamma.widen appid g
    | _ -> Gamma.constr appid ret g

  let apply_signature ?(allow_partial=false) appid ts g {args; rest; ret} =
    let rec apply g ts ns =
      match ts,ns with
      | ts,[] -> Some g,ts
      | [],_ when allow_partial -> Some g, ts
      | [],_ -> None,[]
      | t :: ts, n :: ns -> apply (Gamma.constr t.id n g) ts ns in
    match apply g ts args with
    | None,_ -> None
    | Some g,ts ->
      let g = apply_return appid ret g in
      match ts with
      | [] -> Some g
      | ts -> match rest with
        | None -> None
        | Some typ ->
          Some (List.fold ts ~init:g ~f:(fun g t ->
              Gamma.constr t.id typ g))

  let type_of_expr g expr : typ =
    match Gamma.get g expr.id with
    | None -> Any
    | Some ts -> match Tvals.result ts with
      | Ok Tsym -> Symbol
      | Ok (Grnd n) -> Type n
      | _ -> Any

  let type_of_exprs gamma exprs =
    List.map exprs ~f:(type_of_expr gamma)

  let signature_of_gamma def gamma = {
    rest = None;
    ret = type_of_expr gamma (Def.Func.body def);
    args = type_of_exprs gamma (Def.Func.args def);
  }

  let prenex_type id : typ -> typ = function
    | Name n -> Name (asprintf "%s$%a" n Id.pp id)
    | t -> t

  let prenex_signature id {args; rest; ret}= {
    args = List.map args ~f:(prenex_type id);
    rest = Option.map rest ~f:(prenex_type id);
    ret = prenex_type id ret;
  }

  let signatures glob id gamma name =
    match Map.find glob.prims name with
    | Some sign -> [prenex_signature id sign]
    | None -> match Map.find glob.funcs name with
      | None -> []
      | Some funcs ->
        List.map funcs ~f:(fun def ->
            signature_of_gamma def gamma)


  let apply glob id name args gamma =
    let sigs = signatures glob id gamma name in
    List.filter_map sigs ~f:(fun s ->
        apply_signature id args gamma s) |>
    List.hd |> function
    | None ->
      Gamma.fail id (Unresolved_function (id, KB.Name.show name,sigs)) gamma
    | Some gamma -> gamma

  let last xs = match List.rev xs with
    | {id} :: _ -> id
    | _ -> assert false

  let constr_glob {globs; paras} vars var gamma =
    let name = var.data.exp in
    if Map.mem vars name then gamma
    else match Map.find globs name with
      | Some n -> Gamma.constr var.id (Type n) gamma
      | None ->
        if Set.mem paras name then gamma
        else Gamma.fail var.id
            (Unresolved_variable (var.id,KB.Name.show name)) gamma

  let is_special var =
    let var = KB.Name.unqualified var in
    String.is_prefix ~prefix:"*" var &&
    String.is_suffix ~suffix:"*" var

  let constr_special {paras} var gamma =
    let name = var.data.exp in
    if is_special name && not (Set.mem paras name)
    then Gamma.fail var.id
        (Unresolved_parameter (var.id,KB.Name.show name)) gamma
    else gamma

  let push vars {data; id} =
    Map.set vars ~key:data.exp ~data:id

  let varclass vars v = match Map.find vars v.data.exp with
    | None -> v.id
    | Some id -> id

  let pp_binding ppf (v,id) =
    fprintf ppf "%s:%a" v Id.pp id

  let pp_sep ppf () =
    fprintf ppf ", "

  let pp_vars ppf vs =
    fprintf ppf "{%a}" (pp_print_list ~pp_sep pp_binding) @@
    Map.to_alist vs

  let (++) f g x = f (g x)

  let infer_ast glob bindings ast : Gamma.t -> Gamma.t =
    let rec infer vs expr =
      match expr with
      | {data=Sym _; id} ->
        Gamma.constr id Symbol
      | {data=Int x; id} ->
        Gamma.constr id x.data.typ
      | {data=Var v; id} ->
        Gamma.unify v.id (varclass vs v) ++
        Gamma.unify id  v.id ++
        constr_glob glob vs v ++
        Gamma.constr v.id v.data.typ
      | {data=Ite (x,y,z)} ->
        infer vs x ++
        infer vs y ++
        infer vs z
      | {data=Let (v,x,y); id} ->
        constr_special glob v ++
        Gamma.unify y.id id ++
        Gamma.unify x.id v.id ++
        infer (push vs v) y ++
        infer vs x ++
        Gamma.constr v.id v.data.typ
      | {data=App ((Dynamic name),xs); id} ->
        apply glob id name xs ++
        reduce vs xs
      | {data=Seq []} ->
        ident
      | {data=Seq xs; id} ->
        Gamma.unify (last xs) id ++
        reduce vs xs
      | {data=Set (v,x); id} ->
        Gamma.unify_all id [v.id; x.id; varclass vs v] ++
        Gamma.constr v.id v.data.typ ++
        constr_glob glob vs v ++
        infer vs x
      | {data=Rep (c,x); id} ->
        Gamma.unify c.id id ++
        infer vs c ++
        infer vs x
      | {data=Msg (_,xs); id} ->
        Gamma.constr id (Type 1) ++
        reduce vs xs
      | {data=Err _} -> ident
      | {data=App (Static _,_)} -> ident
    and reduce vs = function
      | [] -> ident
      | x :: xs -> infer vs x ++ reduce vs xs in
    infer bindings ast

  let find_func funcs id =
    Map.to_sequence funcs |>
    Seq.find_map ~f:(fun (_,funcs) ->
        List.find funcs ~f:(fun f -> Id.(f.id = id)))

  let transfer glob node gamma =
    match node with
    | Entry | Exit ->
      gamma
    | Defun id -> match find_func glob.funcs id with
      | None -> gamma
      | Some f ->
        let args = Def.Func.args f in
        let init = Map.empty (module KB.Name) in
        let vars = List.fold args ~init ~f:push in
        let gamma = List.fold args ~init:gamma ~f:(fun gamma v ->
            Gamma.constr v.id v.data.typ gamma) in
        let gamma = infer_ast glob vars (Def.Func.body f) gamma in
        gamma

  let empty_names = Map.empty (module KB.Name)
  let make_globs =
    Seq.fold ~init:empty_names ~f:(fun vars (v,t) -> match t with
        | Type.Imm x ->
          Map.set vars ~key:v ~data:x
        | Type.Mem _
        | Type.Unk -> vars)


  let make_prims {library} init =
    fold_library library ~init ~f:(fun ~package {codes} init ->
        List.fold codes ~init ~f:(fun ps p ->
            match Def.Closure.signature p with
            | None -> ps
            | Some types ->
              let name = KB.Name.create ~package (Def.name p) in
              Map.set ps name types))


  let make_paras {library} =
    fold_library library ~f:(fun ~package {pars} init ->
        List.fold pars ~init ~f:(fun pars par ->
            let name = KB.Name.read ~package (Def.name par) in
            Set.add pars name))
      ~init:(Set.empty (module KB.Name))


  let gamma_equal g1 g2 = Gamma.compare g1 g2 = 0

  let applicable global =
    let filter elts = List.filter elts ~f:(fun def ->
        let def_ctxt =
          Lisp.Attribute.Set.get Lisp.Context.t
            (Def.attributes def) in
        Lisp.Context.(def_ctxt <= global)) in
    Map.map ~f:(fun pkg -> {
          pkg with
          defs = filter pkg.defs;
          mets = filter pkg.mets;
          pars = filter pkg.pars;
        })

  let find_signal library name =
    match Map.find library (KB.Name.package name) with
    | None -> None
    | Some {sigs} ->
      let name = KB.Name.unqualified name in
      List.find sigs ~f:(fun s -> String.equal (Def.name s) name)

  let check_methods glob {context; library} g =
    applicable context library |>
    fold_library ~init:g ~f:(fun ~package {mets} g ->
        List.fold mets ~init:g ~f:(fun g met ->
            let name = Def.name met in
            let fullname = KB.Name.read ~package name in
            match find_signal library fullname with
            | None ->
              Gamma.fail met.id (Unresolved_signal (met.id,name,None)) g
            | Some s ->
              let args = Def.Meth.args met in
              let pars = Def.Signal.signature s in
              match apply_signature ~allow_partial:true met.id args g pars with
              | None ->
                let problem = Unresolved_signal (met.id,name,Some pars) in
                Gamma.fail met.id problem g
              | Some g ->
                let init = Map.empty (module KB.Name) in
                let vars = List.fold args ~init ~f:push in
                let g = List.fold args ~init:g ~f:(fun g v ->
                    Gamma.constr v.id v.data.typ g) in
                infer_ast glob vars (Def.Meth.body met) g))

  let make_defs = fold_library ~f:(fun ~package {defs} funcs ->
      List.fold defs ~init:funcs ~f:(fun funcs def ->
          let name = KB.Name.read ~package (Def.name def) in
          Map.add_multi funcs name def))
      ~init:empty_names

  let infer externals vars (p : program) :  Gamma.t =
    let vars = Seq.map vars ~f:(fun v ->
        KB.Name.read (Var.name v), Var.typ v) in
    let library = applicable p.context p.library in
    let glob = {
      ctxt = p.context;
      prims = make_prims p (Map.of_alist_exn (module KB.Name) externals);
      globs = make_globs vars;
      funcs = make_defs library;
      paras = make_paras p;
    } in
    let g = Callgraph.build library in
    let init = Solution.create Callgraph.Node.Map.empty Gamma.empty in
    let equal = gamma_equal in
    let fp =
      Graphlib.fixpoint (module Callgraph) ~rev:true ~start:Exit
        ~equal ~merge:Gamma.merge ~init ~f:(transfer glob) g in
    let g = Solution.get fp Entry in
    check_methods glob p g

  (* The public interface  *)
  module Type = struct
    type error = {
      sources : Source.t;
      problem : type_error;
    }

    type env = {
      program : program;
      gamma :    Gamma.t;
    }

    let empty = {
      program = empty;
      gamma = Gamma.empty;
    }

    let program env = env.program

    let equal x y = equal x.program y.program
    let merge x y = {
      program = merge x.program y.program;
      gamma = Gamma.merge x.gamma y.gamma;
    }


    let infer ?(externals=[]) vars program =
      let program = Reindex.program program in
      let gamma = infer externals vars program in
      {program; gamma}

    let errors {program={sources}; gamma} =
      Gamma.partition gamma |>
      List.fold ~init:[] ~f:(fun errs g ->
          match Gamma.Group.values g with
          | None -> errs
          | Some vals -> match Tvals.result vals with
            | Ok _ -> errs
            | Error problem -> {sources; problem} :: errs)

    let check vars program : error list = errors (infer vars program)

    let pp_sigs ppf sigs =
      List.iteri sigs ~f:(fun i s ->
          fprintf ppf "%d: %a" (i+1) pp_signature s)

    let pp_val ppf = function
      | Tsym -> fprintf ppf "sym"
      | Grnd n -> fprintf ppf "i%d" n
      | Tany -> fprintf ppf "any"

    let pp_vals_list =
      pp_print_list ~pp_sep:(fun ppf () ->
          fprintf ppf ",")
        pp_val

    let pp_print_underline ppf (off,len) =
      let line = String.init len ~f:(fun i ->
          if i < off then ' ' else '^') in
      fprintf ppf "> %s@\n" line

    let pp_exp ppf (src,exp) =
      let loc = Source.loc src exp in
      fprintf ppf "%a:@\n%a@\n" Loc.pp loc
        (Source.pp_underline src) loc

    let pp_error ppf {sources; problem} = match problem with
      | Unresolved_variable (exp,name) ->
        fprintf ppf "Unresolved variable `%s':@\n%a@\n"
          name pp_exp (sources,exp)
      | Unresolved_function (exp,name,[]) ->
        fprintf ppf "Unresolved function or primitive `%s':@\n%a@\n"
          name pp_exp (sources,exp)
      | Unresolved_function (exp,name,sigs) ->
        fprintf ppf "No matching signature for `%s':@\n%a@\nTried:@\n%a@\n"
          name pp_exp (sources,exp) pp_sigs sigs
      | Unresolved_signal (exp,name,None) ->
        fprintf ppf "Unresolved signal for method `%s':@\n%a\n"
          name pp_exp (sources,exp)
      | Unresolved_signal (exp,name,Some signature) ->
        fprintf ppf "
The signal `%s' has signature `%a'. This signature doesn't match with
the inferred type of the method:@\n%a"
          name pp_signature signature pp_exp (sources,exp)
      | Unresolved_parameter (exp,name) ->
        fprintf ppf "The variable `%s' is treated as parameter, \
                     but there is no such parameter:@\n%a"
          name pp_exp (sources,exp)
      | No_unification (x,t1,y,t2) ->
        fprintf ppf "
Type error: expected %a got %a. Details follow, the expression:
%ahas type %a, but it is expected to have type %a after its unification
with the expression:@\n%a@\n"
          pp_val t1 pp_val t2
          pp_exp (sources,x) pp_val t1 pp_val t2 pp_exp (sources,y)
  end
end

module Context = Lisp.Context
module Type = Typing.Type
