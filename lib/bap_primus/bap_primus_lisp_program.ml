open Bap.Std
open Core_kernel
open Graphlib.Std
open Regular.Std
open Monads.Std
open Bap_primus_lisp_types
open Format


module Lisp = struct
  module Context = Bap_primus_lisp_context
  module Var = Bap_primus_lisp_var
  module Type = Bap_primus_lisp_type
end

module Def = Bap_primus_lisp_def

type t = {
  context : Lisp.Context.t;
  sources : Source.t;
  codes : Def.prim Def.t list;
  macros : Def.macro Def.t list;
  substs : Def.subst Def.t list;
  consts : Def.const Def.t list;
  defs : Def.func Def.t list;
  mets : Def.meth Def.t list;
  pars : Def.para Def.t list;
} [@@deriving fields]

type program = t

let empty = {
  context = Lisp.Context.empty;
  sources = Source.empty;
  codes = [];
  defs = [];
  mets = [];
  pars = [];
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
  let meth = Fields.mets
  let para = Fields.pars
  let primitive = Fields.codes
end

let add p (fld : 'a item) x =
  Field.fset fld p (x :: Field.get fld p)

let get p (fld : 'a item) = Field.get fld p

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

  let empty = String.Set.empty
  let (++) = Set.union
  let call = String.Set.singleton
  let union xs ~f =
    List.map xs ~f |>
    String.Set.union_list

  let rec calls = function
    | {data=App ((Dynamic v),xs); } -> call v ++ union xs ~f:calls
    | {data=(Var _ | Int _ | Sym _ | Err _)} -> empty
    | {data=Ite (x,y,z)} -> calls x ++ calls y ++ calls z
    | {data=(Seq xs | App (_,xs) | Msg (_,xs))} -> union xs ~f:calls
    | {data=(Let (_,x,y) | Rep (x,y))} -> calls x ++ calls y
    | {data=Set (_,x)} -> calls x


  (** computes a mapping from name to id of definitions  *)
  let compute_ids defs =
    List.fold defs ~init:String.Map.empty ~f:(fun ids def ->
        Map.add_multi ids ~key:(Def.name def) ~data:def.id)

  let edge id id' =
    G.Edge.create (Defun id) (Defun id') ()

  let build_kernel defs =
    let ids =
      let ids = compute_ids defs in
      fun name -> match Map.find ids name with
        | None -> []
        | Some x -> x in
    List.fold defs ~init:G.empty ~f:(fun g def ->
        let g = G.Node.insert (Defun def.id) g in
        Set.fold (calls (Def.Func.body def)) ~init:g ~f:(fun g name ->
            List.fold (ids name) ~init:g ~f:(fun g id ->
                G.Edge.insert (edge def.id id) g)))

  let close dir g =
    let edge n = match dir with
      | `In -> G.Edge.create Entry n ()
      | `Out -> G.Edge.create n Exit () in
    G.nodes g |> Seq.fold ~init:g ~f:(fun g n ->
        if G.Node.degree ~dir n g = 0
        then G.Edge.insert (edge n) g
        else g)

  let build defs =
    close `Out (close `In (build_kernel defs))

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
    fprintf ppf "%a" pp_exp exp
  | {data={exp; typ}} ->
    fprintf ppf "%a:%a" pp_exp exp Lisp.Type.pp typ
let pp_word = pp_term Int64.pp
let pp_var = pp_term String.pp

let rec concat_prog p =
  List.concat_map p ~f:(function
      | {data=Seq xs} -> concat_prog xs
      | x -> [x])

module Ast = struct
  let rec pp ppf {data} = pp_exp ppf data
  and pp_exp ppf = function
    | Int x ->
      pp_word ppf x
    | Sym x ->
      pp_print_string ppf x.data
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

let pp ppf {pars; mets; defs;} =
  let pp_items pp items =
    fprintf ppf "@[<v>%a@]" (pp_print_list pp) items in
  pp_items pp_par pars;
  pp_items pp_met mets;
  pp_items pp_def defs


module Use = struct
  let empty = String.Map.empty
  let union = union empty
  let use = String.Map.singleton

  type t = {
    calls : Id.t String.Map.t Id.Map.t;
    vars  : Id.t String.Map.t Id.Map.t; (* def -> var -> use  *)
  }

  let vars bound ast =
    let use bound {exp=v} id =
      if Set.mem bound v then String.Map.empty
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
                    String.Set.of_list in
        let vs = vars bound body in
        let cs = calls body in
        {
          calls = Map.set s.calls ~key:def.id ~data:cs;
          vars = Map.set s.vars ~key:def.id ~data:vs;
        })
end

(** Assign fresh indices to trees that were produced my macros or that
 ** has no indices at all.
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
module Reindex = struct
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

  let derive from =
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

  let reindex_all p =
    let def = Def.Func.body,Def.Func.with_body in
    let met = Def.Meth.body,Def.Meth.with_body in
    let par = Def.Para.default,Def.Para.with_default in
    let macros = macro_ids p in
    State.List.map p.defs ~f:(reindex def macros) >>= fun defs ->
    State.List.map p.mets ~f:(reindex met macros) >>= fun mets ->
    State.List.map p.pars ~f:(reindex par macros) >>= fun pars ->
    State.return (defs,mets,pars)

  let program p =
    let (defs,mets,pars),sources =
      State.run (reindex_all p) p.sources in
    {p with defs; mets; pars; sources}

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
    | Tsym
    | Grnd of int
  [@@deriving compare, sexp_of]

  module Tval = Comparable.Make_plain(struct
      type t = tval [@@deriving compare, sexp_of]
    end)

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

    val empty : t

    (** [get gamma exp] returns a type of the expression [exp].
        If [None] is returned, then the expression doesn't have any
        statical constraints, so its type is [Any]. If some set is
        returned, then this set denotes a disjunction of types, that a
        term can have during program evaluation. If this set is empty,
        then the term is ill-typed.
    *)
    val get : t -> Id.t -> Tval.Set.t option


    val merge : t -> t -> t

    (** [exps gamma] returns a list of typed expressions. *)
    val exps : t -> Id.t list

    (** [constr exp typ] expression [exp] shall have type [typ]  *)
    val constr : Id.t -> typ -> rule

    (** [meet x y] types of [x] and [y] shall have the same type *)
    val meet : Id.t -> Id.t -> rule

    (** [join x ys] expression [x] shall have a type that is a
        disjunction of the types of expressions specified by the [ys] list.  *)
    val join : Id.t -> Id.t list -> rule

  end = struct
    (* typing environment.

       [vars] associates each program term with a type variable. It is a
       disjoint set that partitions the set of program terms into
       equivalence classes, such that two terms belonging to the same
       set will have the same type.

       [vals] is the typing environment that associates each type
       variable with the sum of type values (ground types). If an type
       variable is not mapped in [vals] then it is assumed to has type
       Top (i.e., it is a set of all possible types). An empty set
       denotes the bottom type, i.e., all expressions that has that type
       are ill-typed.*)
    type t = {
      vars : tvar Id.Map.t;
      vals : Tval.Set.t Tvar.Map.t;
    } [@@deriving compare, sexp_of]

    type rule = t -> t

    let empty = {
      vars = Id.Map.empty;
      vals = Tvar.Map.empty;
    }

    let exps {vars} = Map.keys vars

    let merge g g' = {
      vars = Map.merge g.vars g'.vars ~f:(fun ~key:_ -> function
          | `Left t | `Right t -> Some t
          | `Both (_,t') -> Some t');
      vals = Map.merge g.vals g'.vals ~f:(fun ~key:_ -> function
          | `Left ts | `Right ts -> Some ts
          | `Both (_,ts) -> Some ts)

    }


    let add_var id t g =
      {g with vars = Map.set g.vars ~key:id ~data:t}

    let add_val t v g =
      {g with vals = Map.set g.vals ~key:t ~data:v}

    let unify t1 t2 g =
      let t = Tvar.min t1 t2 in
      match Map.find g.vals t1, Map.find g.vals t2 with
      | None, None -> g
      | None, Some v | Some v, None -> add_val t v g
      | Some v, Some v' -> add_val t (Set.inter v v') g

    let meet id1 id2 g =
      let t,g = match Map.find g.vars id1, Map.find g.vars id2 with
        | None,None -> Tvar.min (Tvar id1) (Tvar id2),g
        | None,Some t | Some t,None -> t,g
        | Some u,Some v -> Tvar.min u v, unify u v g in
      add_var id2 t @@
      add_var id1 t g


    let get g id = match Map.find g.vars id with
      | None -> None
      | Some rep -> Map.find g.vals rep


    let inter_list = function
      | [] -> None
      | x :: xs -> Some (List.fold ~init:x xs ~f:Set.inter)

    let join id ids g =
      List.filter_map ids ~f:(get g) |>
      inter_list |> function
      | None -> g
      | Some vs -> match Map.find g.vars id with
        | None -> {
            vars = Map.set g.vars ~key:id ~data:(Tvar id);
            vals = Map.set g.vals ~key:(Tvar id) ~data:vs;
          }
        | Some v -> {
            g with vals = Map.update g.vals v ~f:(function
            | None -> vs
            | Some vs' -> Set.union vs vs')
          }

    let constr_name id n g =
      let g' = add_var id (Name n) g in
      match Map.find g.vars id with
      | None -> g'
      | Some v -> Map.fold g'.vars ~init:g ~f:(fun ~key:id' ~data:v' g ->
          if Tvar.equal v v'
          then meet id id' g
          else g)

    let constr_grnd id t g =
      let v = match Map.find g.vars id with
        | None -> Tvar id
        | Some v -> v in
      let g = add_var id v g in
      let t = Tval.Set.singleton t in
      {g with
       vals = Map.update g.vals v ~f:(function
           | None -> t
           | Some t' -> Set.inter t t')
      }

    let constr id t g = match t with
      | Any -> g
      | Name n -> constr_name id n g
      | Symbol -> constr_grnd id Tsym g
      | Type n -> constr_grnd id (Grnd n) g

  end

  type signature = Lisp.Type.signature = {
    args : typ list;
    rest : typ option;
    ret  : typ;
  }

  type t = {
    ctxt : Lisp.Context.t;
    globs : int String.Map.t;
    prims : signature String.Map.t;
    funcs : Def.func Def.t list;
  }


  let pp_args ppf args =
    pp_print_list Lisp.Type.pp ppf args

  let pp_signature ppf {args; rest; ret} =
    fprintf ppf "@[(%a" pp_args args;
    Option.iter rest ~f:(fun rest ->
        fprintf ppf "&rest %a" Lisp.Type.pp rest);
    fprintf ppf ")@] => (%a)" Lisp.Type.pp ret

  let pp_tval ppf = function
    | Tsym -> fprintf ppf "sym"
    | Grnd n -> fprintf ppf "%d" n

  let pp_plus ppf () = pp_print_char ppf '+'
  let pp_tvals ppf tvals =
    if Set.is_empty tvals
    then fprintf ppf "nil"
    else fprintf ppf "%a"
        (pp_print_list ~pp_sep:pp_plus pp_tval)
        (Set.elements tvals)

  let apply_signature appid ts g {args; rest; ret} =
    let rec apply g ts ns =
      match ts,ns with
      | ts,[] -> Some g,ts
      | [],_ -> None,[]
      | t :: ts, n :: ns -> apply (Gamma.constr t.id n g) ts ns in
    match apply g ts args with
    | None,_ -> None
    | Some g,ts ->
      let g = Gamma.constr appid ret g in
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
    | Some ts -> match Set.elements ts with
      | [Tsym] -> Symbol
      | [Grnd n] -> Type n
      | _ -> Any

  let type_of_exprs gamma exprs =
    List.map exprs ~f:(type_of_expr gamma)

  let signature_of_gamma def gamma = {
    rest = None;
    ret = type_of_expr gamma (Def.Func.body def);
    args = type_of_exprs gamma (Def.Func.args def);
  }

  let signatures glob gamma name =
    match Map.find glob.prims name with
    | Some sign -> [sign]
    | None -> List.fold glob.funcs ~init:[] ~f:(fun sigs def ->
        if String.equal (Def.name def) name
        then signature_of_gamma def gamma :: sigs
        else sigs)

  let join_gammas xs _why_is_it_ignored = xs

  let apply glob id name args gamma =
    signatures glob gamma name |>
    List.filter_map ~f:(apply_signature id args gamma) |>
    List.reduce ~f:join_gammas |> function
    | None -> gamma
    | Some gamma -> gamma

  let last xs = match List.rev xs with
    | {id} :: _ -> id
    | _ -> assert false

  let constr_glob {globs} vars var gamma =
    if Map.mem vars var.data.exp then gamma
    else match Map.find globs var.data.exp with
      | None -> gamma
      | Some n -> Gamma.constr var.id (Type n) gamma


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
        Gamma.meet v.id (varclass vs v) ++
        Gamma.meet id  v.id ++
        constr_glob glob vs v ++
        Gamma.constr v.id v.data.typ
      | {data=Ite (x,y,z); id} ->
        Gamma.join id [y.id; z.id] ++
        infer vs x ++
        infer vs y ++
        infer vs z
      | {data=Let (v,x,y); id} ->
        Gamma.meet y.id id ++
        infer (push vs v) y ++
        Gamma.meet x.id v.id ++
        infer vs x ++
        Gamma.constr v.id v.data.typ
      | {data=App ((Dynamic name),xs); id} ->
        apply glob id name xs ++
        reduce vs xs
      | {data=Seq []} -> ident
      | {data=Seq xs; id} ->
        Gamma.meet (last xs) id ++
        reduce vs xs
      | {data=Set (v,x); id} ->
        Gamma.join id [v.id; x.id] ++
        Gamma.constr v.id v.data.typ ++
        infer vs x
      | {data=Rep (c,x); id} ->
        Gamma.meet c.id id ++
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
    List.find funcs ~f:(fun f -> Id.(f.id = id))

  let transfer glob node gamma =
    match node with
    | Entry | Exit ->
      gamma
    | Defun id -> match find_func glob.funcs id with
      | None -> gamma
      | Some f ->
        let args = Def.Func.args f in
        let vars = List.fold args ~init:String.Map.empty ~f:push in
        let gamma = List.fold args ~init:gamma ~f:(fun gamma v ->
            Gamma.constr v.id v.data.typ gamma) in
        let gamma = infer_ast glob vars (Def.Func.body f) gamma in
        gamma

  let make_globs =
    Seq.fold ~init:String.Map.empty ~f:(fun vars v ->
        match Var.typ v with
        | Type.Imm x ->
          Map.set vars ~key:(Var.name v) ~data:x
        | Type.Mem _
        | Type.Unk -> vars)

  let make_prims {codes} =
    List.fold codes ~init:String.Map.empty ~f:(fun ps p ->
        match Def.Closure.signature p with
        | None -> ps
        | Some types -> Map.set ps ~key:(Def.name p) ~data:types)

  let gamma_equal g1 g2 = Gamma.compare g1 g2 = 0

  let infer vars p : Gamma.t =
    let glob = {
      ctxt = p.context;
      prims = make_prims p;
      globs = make_globs vars;
      funcs = p.defs;
    } in
    let g = Callgraph.build p.defs in
    let init = Solution.create Callgraph.Node.Map.empty Gamma.empty in
    let equal = gamma_equal in
    let fp =
      Graphlib.fixpoint (module Callgraph) ~rev:true ~start:Exit
        ~equal ~merge:Gamma.merge ~init ~f:(transfer glob) g in
    Solution.get fp Entry

  (* The public interface  *)
  module Type = struct
    type error = Loc.t * Source.Id.t
    let check vars p : error list =
      let p = Reindex.program p in
      let gamma = infer vars p in
      List.fold (Gamma.exps gamma) ~init:Loc.Map.empty ~f:(fun errs exp ->
          assert Id.(exp <> Source.Id.null);
          if Source.has_loc p.sources exp
          then match Gamma.get gamma exp with
            | None -> errs
            | Some ts ->
              if Set.is_empty ts
              then Map.set errs ~key:(Source.loc p.sources exp) ~data:exp
              else errs
          else errs) |>
      Map.to_alist

    let pp_error ppf (loc,id) =
      fprintf ppf "%a@\nType error - expression is ill-typed: %a"
        Loc.pp loc Id.pp id
  end
end



module Context = Lisp.Context
module Type = Typing.Type
