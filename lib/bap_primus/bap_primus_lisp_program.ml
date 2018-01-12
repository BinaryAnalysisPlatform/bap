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
} [@@deriving fields]

type program = t

let empty = {
  context = Lisp.Context.empty;
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

let (++) = Map.merge ~f:(fun ~key -> function
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
  | {data={exp; typ=Any}; id} -> 
    fprintf ppf "%a.%a" pp_exp exp Id.pp id
  | {data={exp; typ}; id} -> 
    fprintf ppf "%a:%a.%a" pp_exp exp Lisp.Type.pp typ Id.pp id
let pp_word = pp_term Int64.pp
let pp_var = pp_term String.pp

let rec concat_prog =
  List.concat_map ~f:(function
      | {data=Seq xs} -> concat_prog xs
      | x -> [x])

let concat_prog = ident

module Ast = struct
  let rec pp ppf {data; id} = 
    fprintf ppf "%a.%a" pp_exp data Source.Id.pp id
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
    | {data=Seq xs; id} ->
      fprintf ppf "%a.%a" pp_exps (concat_prog xs) Id.pp id
    | exp -> pp ppf exp
end

let pp_def ppf d =
  fprintf ppf "@[<2>(defun %s @[<2>(%a)@]@ %a)@]@,"
    (Def.name d)
    (pp_print_list ~pp_sep:pp_print_space pp_var) (Def.Func.args d)
    Ast.pp_prog (Def.Func.body d)

let pp ppf {defs} =
  fprintf ppf "Printing %d definitions@\n" (List.length defs);
  fprintf ppf "@[<v>%a@]" (pp_print_list pp_def) defs

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
          calls = Map.add s.calls ~key:def.id ~data:cs;
          vars = Map.add s.vars ~key:def.id ~data:vs;
        })
end

module Reindex = struct
  module State = Monad.State.Make(Source)(Monad.Ident)
  open State.Syntax
  type 'a m = 'a Monad.State.T1(Source)(Monad.Ident).t


  let rec ids_of_trees trees =
    List.fold trees ~init:Id.Set.empty ~f:(fun xs t -> match t with
        | {data=Atom v; id} -> Set.add xs id
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

  let reindex_def macros def =
    let rename t =
      if Set.mem macros t.id || Id.null = t.id
      then derive t.id >>| fun id -> {t with id}
      else State.return t in
    let rec map t : ast m =
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
    and map_all = State.List.map ~f:map in
    map (Def.Func.body def) >>|
    Def.Func.with_body def

  let reindex p =
    let macros = macro_ids p in
    State.List.map p.defs ~f:(reindex_def macros)

  let program p =
    let defs,sources = State.run (reindex p) p.sources in
    {p with defs; sources}

end

module Typing = struct
  type texpr =
    | Tsym
    | Grnd of int
    | Meet of texpr * texpr
    | Join of texpr * texpr
    | Tvar of Id.t
  [@@deriving compare]

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
  type gamma = texpr Id.Map.t

  let empty = Id.Map.empty
  let merge = union empty
  let bind id typ = Id.Map.singleton id typ
  let get gamma id = match Map.find gamma id with
    | None -> Tvar id
    | Some t -> t

  let rec pp_expr ppf = function
    | Tsym -> fprintf ppf "symbol"
    | Grnd x -> fprintf ppf "%d" x
    | Meet (x,y) -> fprintf ppf "%a=%a" pp_expr x pp_expr y
    | Join (x,y) -> fprintf ppf "%a:%a" pp_expr x pp_expr y
    | Tvar id -> fprintf ppf "t%a" Id.pp id

  let pp_args ppf args =
    pp_print_list Lisp.Type.pp ppf args

  let pp_signature ppf {args; rest; ret} =
    fprintf ppf "@[(%a" pp_args args;
    Option.iter rest ~f:(fun rest ->
        fprintf ppf "&rest %a" Lisp.Type.pp rest);
    fprintf ppf ")@] => (%a)" Lisp.Type.pp ret

  let make_join t t' = match t,t' with
    | Join (x,y), z when x = z || y = z -> t
    | _ -> if t = t' then t else Join (t,t')

  let make_meet t t' = match t,t' with
    | Join (x,y),z when x = z || y = z -> z
    | _ -> if t = t' then t else Meet (t,t')

  let join id1 id2 id gamma =
    gamma ++ bind id (make_join (get gamma id1) (get gamma id2))


  let make_texpr id = function
    | Any | Name _ -> Tvar id
    | Type n -> Grnd n
    | Symbol -> Tsym

  (* substitute all occurences of the type variable [v] (denoted by
     its id) with type [t].  *)
  let subst v t g = 
    let rec subst x = 
      eprintf "subst %a %a@\n%!" Id.pp v pp_expr x;
      match x with
      | Meet (t1,t2) -> make_meet (subst t1) (subst t2)
      | Join (t1,t2) -> make_join (subst t1) (subst t2)
      | Tvar v' as t' -> if v = v' then t else t'
      | Tsym | Grnd _ as t -> t in
    Map.map ~f:subst g

  let unify_meets g =
    Map.fold g ~init:g ~f:(fun ~key:id ~data:t g ->
        match t with
        | Meet (Tvar x, t')
        | Meet (t', Tvar x) -> subst x t' g
        | _ -> g)


  (* constrains the type of expression [expr] (denoted with its id) to
     type [t].  If the expression wasn't constrained then it is added
     as a new constraint to the typing context. If expression was
     constrained to some type variable, then all occurences of this
     type variable in the typing context are substituted with type
     [t]. If it was constrained with some other type, then it will be
     constrained with the greatest lower bound of the old and new
     types.  
  *)

  let constr expr t gamma = 
    eprintf "constr %a %a@\n%!" Id.pp expr pp_expr t;
    match get gamma expr with
    | Tvar id' -> 
      subst id' t (Map.add gamma ~key:expr ~data:t)
    | t' -> Map.add gamma ~key:expr ~data:(make_meet t t')


  (* unifies types of expressions using function [f] *)
  let merge ~f ids gamma = match ids with
    | [] -> gamma
    | id :: ids -> 
      let t = List.fold ids ~init:(get gamma id) ~f:(fun t id -> 
          f (get gamma id) t) in
      List.fold (id::ids) ~init:gamma ~f:(fun gamma id -> 
          constr id t gamma) |> unify_meets

  (* unifies types of expressions denoted with [ids] with 
     a greatest lower bound of their types *)
  let meet = merge ~f:make_meet

  let join expr ids gamma = match ids with
    | [] -> gamma
    | id :: ids -> 
      let t = List.fold ids ~init:(get gamma id) ~f:(fun t id -> 
          make_join (get gamma id) t) in
      constr expr t gamma


  let meet_with_symbol expr g = constr expr Tsym g
  let meet_with_ground expr n g = constr expr (Grnd n) g

  let apply_ret expr gamma vs : typ -> gamma = function
    | Any -> gamma
    | Symbol -> constr expr Tsym gamma
    | Type n -> meet_with_ground expr n gamma
    | Name n -> match Map.find vs n with
      | None -> gamma
      | Some id' -> constr expr (get gamma id') gamma

  let apply_signature appid ts g {args; rest; ret} =
    let rec apply g vs ts ns = 
      eprintf "apply arg@\n%!";
      match ts,ns with
      | ts,[] -> Some g,vs,ts
      | [],_ -> None,vs,[]
      | _ :: ts, Any :: ns -> apply g vs ts ns
      | t :: ts, Type n :: ns ->
        apply (meet_with_ground t.id n g) vs ts ns
      | t :: ts, Symbol :: ns ->
        apply (meet_with_symbol t.id g) vs ts ns
      | t :: ts, Name n :: ns -> match Map.find vs n with
        | Some id -> apply (constr t.id (Tvar id) g) vs ts ns
        | None -> apply g (Map.add vs ~key:n ~data:t.id) ts ns in
    match apply g String.Map.empty ts args with
    | None,_,_ -> None
    | Some g,vs,ts ->
      let g = apply_ret appid g vs ret in
      match ts with
      | [] -> Some g
      | ts -> match rest with
        | None -> None
        | Some Any -> Some g
        | Some Symbol -> 
          Some (List.fold ts ~init:g ~f:(fun g t -> 
              meet_with_symbol t.id g))
        | Some (Type n) ->
          Some (List.fold ts ~init:g ~f:(fun g t ->
              meet_with_ground t.id n g))
        | Some (Name n) -> match Map.find vs n with
          | None -> Some g
          | Some id ->
            Some (List.fold ts ~init:g ~f:(fun g t ->
                constr t.id (Tvar id) g))

  let type_of_expr gamma expr = match Map.find gamma expr.id with
    | Some (Grnd n) -> Type n
    | Some Tsym -> Symbol
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
        if Def.name def = name
        then signature_of_gamma def gamma :: sigs
        else sigs)

  let join_gammas xs ys = 
    let zs = Map.merge xs ys ~f:(fun ~key -> function
        | `Left t | `Right t -> Some (make_join t (Tvar key))
        | `Both (t,t') -> Some (make_join t t')) in
    zs

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
      | Some n ->
        (* TODO: shouldn't we meet here? *)
        Map.add gamma ~key:var.id ~data:(Grnd n)

  let push vars {data; id} =
    Map.add vars ~key:data.exp ~data:id

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

  let infer_ast glob bindings ast : gamma -> gamma =
    let rec infer vs expr =
      let (++) f g x = 
        eprintf "infer %a s.t. %a@\n%!" Ast.pp expr pp_vars vs;
        f (g x) in
      match expr with
      | {data=Sym _; id} -> meet_with_symbol id
      | {data=Int x; id} -> constr id (make_texpr id x.data.typ)
      | {data=Var v; id} ->
        meet [v.id; varclass vs v; id] ++
        constr_glob glob vs v ++
        constr v.id (make_texpr id v.data.typ)
      | {data=Ite (x,y,z); id} ->
        join id [y.id; z.id] ++
        infer vs x ++
        infer vs y ++
        infer vs z
      | {data=Let (v,x,y); id} ->
        meet [y.id; id] ++
        infer (push vs v) y ++
        meet [x.id; v.id] ++
        infer vs x ++
        constr v.id (make_texpr v.id v.data.typ)
      | {data=App ((Dynamic name),xs); id} ->
        apply glob id name xs ++
        reduce vs xs
      | {data=Seq []; id} -> ident
      | {data=Seq xs; id} ->
        meet [last xs; id] ++
        reduce vs xs
      | {data=Set (v,x); id} ->
        join id [v.id; x.id] ++
        constr v.id (make_texpr v.id v.data.typ) ++
        infer vs x
      | {data=Rep (c,x); id} ->
        meet [c.id; id] ++
        infer vs c ++
        infer vs x
      | {data=Msg (_,xs); id} ->
        constr id (Grnd 1) ++
        reduce vs xs
      | {data=Err _; id} -> ident
      | {data=App (Static _,_)} -> ident
    and reduce vs = function
      | [] -> ident
      | x :: xs -> infer vs x ++ reduce vs xs in
    infer bindings ast

  let find_func funcs id =
    List.find funcs ~f:(fun f -> f.id = id)

  let pp_gbinding ppf (id,t) = 
    fprintf ppf "%a:%a" Id.pp id pp_expr t

  let pp_gamma ppf gamma = 
    fprintf ppf "{@[<v>%a@]}@\n"
      (pp_print_list pp_gbinding) (Map.to_alist gamma)

  let transfer glob node gamma =
    match node with
    | Entry | Exit ->
      gamma
    | Defun id -> match find_func glob.funcs id with
      | None -> gamma
      | Some f ->
        eprintf "typing %s@\n%!" (Def.name f);
        let args = Def.Func.args f in
        let vars = List.fold args ~init:String.Map.empty ~f:push in
        let gamma = List.fold args ~init:gamma ~f:(fun gamma v ->
            constr v.id (make_texpr v.id v.data.typ) gamma) in
        let gamma = infer_ast glob vars (Def.Func.body f) gamma in
        eprintf "finished@\n%!";
        gamma


  let meet g1 g2 =
    unify_meets @@ Map.merge g1 g2 ~f:(fun ~key -> function
        | `Left t | `Right t -> Some t
        | `Both (t,t') -> Some (make_meet t t')) 


  let equal_texpr x y = compare_texpr x y = 0
  let equal = Id.Map.equal equal_texpr

  let make_globs =
    Seq.fold ~init:String.Map.empty ~f:(fun vars v ->
        match Var.typ v with
        | Type.Imm x ->
          Map.add vars ~key:(Var.name v) ~data:x
        | Type.Mem _ -> vars)

  let make_prims {codes} =
    List.fold codes ~init:String.Map.empty ~f:(fun ps p ->
        match Def.Closure.signature p with
        | None -> ps
        | Some types -> Map.add ps ~key:(Def.name p) ~data:types)


  let infer vars p : gamma =
    let glob = {
      ctxt = p.context;
      prims = make_prims p;
      globs = make_globs vars;
      funcs = p.defs;
    } in
    let g = Callgraph.build p.defs in
    let init = Solution.create Callgraph.Node.Map.empty Id.Map.empty in
    let fp =
      Graphlib.fixpoint (module Callgraph) ~rev:true ~start:Exit
        ~equal ~merge:meet ~init ~f:(transfer glob) g in
    Solution.get fp Entry

  let rec well_typed = function
    | Meet (Tsym,Tsym) -> true
    | Meet (Grnd x, Grnd y) -> x = y
    | Meet (Grnd _, Tsym)
    | Meet (Tsym, Grnd _) -> false
    | Meet (x,y) -> well_typed x && well_typed y
    | Join (x,y) -> well_typed x || well_typed y
    | Tvar _ | Grnd _ | Tsym -> true


  (* The public interface  *)
  module Type = struct
    type error = Loc.t * (Source.Id.t * texpr)
    let check vars p =
      let p = Reindex.program p in
      let gamma = infer vars p in
      Map.fold gamma ~init:Loc.Map.empty ~f:(fun ~key:id ~data:t errs ->
          assert (id <> Source.Id.null);
          if well_typed t || not (Source.has_loc p.sources id) then errs
          else Map.add errs ~key:(Source.loc p.sources id) ~data:(id,t)) |>
      Map.to_alist

    let pp_error ppf (loc,(id,expr)) =
      fprintf ppf "%a@\nType error - expression %a is ill-typed: %a"
        Loc.pp loc Id.pp id pp_expr expr
  end
end



module Context = Lisp.Context
module Type = Typing.Type

