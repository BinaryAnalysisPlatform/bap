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
    | {data=App ((Dynamic v),_); } -> call v
    | {data=(Var _ | Int _ | Err _)} -> empty
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
      | {data=(Int _ | Err _)}  -> empty
      | {data=Var v; id} -> use bound v.data id
      | {data=Ite (x,y,z)} -> free bound x ++ free bound y ++ free bound z
      | {data=Let (v,x,y)} -> free bound x ++ free (Set.add bound v.data.exp) y
      | {data=(Seq xs | App (_,xs) | Msg (_,xs))}  -> union xs ~f:(free bound)
      | {data=Set (v,x); id} -> use bound v.data id ++ free bound x
      | {data=Rep (x,y)} -> free bound x ++ free bound y in
    free bound ast

  let rec calls = function
    | {data=App ((Dynamic v),_); id} -> use v id
    | {data=(Var _ | Int _ | Err _)} -> empty
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

module type Fixpoint = sig
  type ('n,'d) t

  val compute : (module Graph with type t = 'c
                               and type node = 'n) ->
    ?steps:int -> ?start:'n -> ?rev:bool ->
    meet:('d -> 'd -> 'd) -> init:'d -> f:('n -> 'd -> 'd) -> 'c -> ('n,'d) t

  val iterations : ('n,'d) t -> int
  val is_maximal : ('n,'d) t -> bool
  val solution : ('n,'d) t -> 'n -> 'd

end

module Fixpoint = struct
  type ('n,'d) t = Solution : {
      steps : int option;
      iters : int;
      init : 'd;
      approx : ('n,'d,_) Map.t;
    } -> ('n,'d) t

  let solution (Solution {approx; init}) n =
    match Map.find approx n with
    | None -> init
    | Some x -> x

  let is_maximal (Solution {steps; iters}) = match steps with
    | None -> true
    | Some steps -> iters < steps

  type ('a,'b) step =
    | Step of 'a
    | Done of 'b

  let continue x = Step x


  (* Kildall's Worklist Algorithm Implementation


     Kildall's Algorithm
     ===================

     Pseudocode:

     Given a set of node W, and a finite mapping A from nodes to
     approximations, a function F, the initial approximation I, and a
     start node B, the algorithm refines the mapping A, until a
     fixpoint is reached.


     {v
     let W = {B}
     for each node N in graph G:
        A[N] := I

     while W <> {}:
        pop a node N from W
        let OUT = F N A[N]
        for each successor S of N:
           let IN = A[S] /\ OUT
           if IN <> A[S]:
              A[S] := IN
              W := union(W,{S}
        end
     end
     v}


     If the meet operation (/\) induces a partial order over the set
     of approximations, and function F is monotonic, then the result
     would me the maximal fixpoint solution.


     Implementation
     ==============

     1. We do not distinguish between forward and backward problems,
     since a backward problem can be expressed as forward, on the
     reversed graph and inversed lattice. Thus, we express our
     algorithm as a forward problem with a meet semilattice. We do not
     require, of course, a user to provide a reverse graph, instead the
     flag [rev] could be used to virtually reverse the graph, and
     the exit node should be provided as a start node.

     2. Since the algorithm converges faster if a worklist is
     traversed in the reverse postorder we rank the graph nodes with
     their reverse postorder (rpost) numbers and use an array [nodes]
     for fast mapping from rpost numbers to nodes. We then represent
     the worklist as an integer set, and always pick the minimal
     element from the worklist.

     3. We also precompute a set of successors [succs] (as a set of
     their rpost numbers) for each node.

     4. In the loop body we use rpost numbers as node representations,
     and the finite mapping A is a mapping from integers to
     approximations.

     5. We optionally bound our loop with the maximum number of
     iterations, allowing an algorithm to terminate before it
     converges. Thus the result might be not a maximal fixpoint (i.e.,
     it might not be the greater lower bound for some if not all
     nodes, however, it should still be the over-approximation, given
     the correct meet, f,  and initial approximation.


     Caveats
     =======

     1. We allow only one start node. If it is specified, then only
     those nodes that are reachable from the start node will
     participate in the computation. So, unless, it is really desired,
     it is a good idea to introduce an artificial entry node, from
     which everything is reachable.

  *)
  let compute
      (type g n)
      (module G : Graph with type t = g and type node = n)
      ?steps ?start ?(rev=false) ~equal ~meet ~init ~f g =
    let nodes =
      Graphlib.reverse_postorder_traverse (module G) ~rev ?start g |>
      Sequence.to_array in
    eprintf "Got %d nodes |G| = %d@\n" (Array.length nodes)
      (G.number_of_nodes g);
    let rnodes =
      Array.foldi nodes ~init:G.Node.Map.empty ~f:(fun i rnodes n ->
          Map.add rnodes ~key:n ~data:i) in
    let succs = Array.map nodes ~f:(fun n ->
        let succs = if rev then G.Node.preds else G.Node.succs in
        succs n g |> Sequence.fold ~init:Int.Set.empty ~f:(fun ns n ->
            match Map.find rnodes n with
            | None -> ns
            | Some i -> Set.add ns i)) in
    let get approx n = match Map.find approx n with
      | None -> init
      | Some x -> x in
    let step works approx = match Set.min_elt works with
      | None ->
        eprintf "No more nodes in the work list@\n";
        Done approx
      | Some n ->
        let works = Set.remove works n in
        let out = f nodes.(n) (get approx n) in
        eprintf
          "Computed approximation for node %d, procceding to %d successors@\n"
          n (Set.length succs.(n));
        succs.(n) |>
        Set.fold ~init:(works,approx) ~f:(fun (works,approx) n ->
            let ap = get approx n in
            let ap' = meet out ap in
            eprintf "%d /\\ %d = %d@\n"
              (Map.length out) (Map.length ap) (Map.length ap');
            if equal ap ap'
            then eprintf "Nothing new for successor %d@\n" n
            else eprintf "Will recompute successor %d@\n" n;

            if equal ap ap' then (works,approx)
            else Set.add works n,
                 Map.add approx ~key:n ~data:ap') |>
        continue in
    let can_iter iters = match steps with
      | None -> true
      | Some steps -> iters < steps in
    let make_solution iters approx = Solution {
        steps;
        iters;
        init;
        approx = Map.fold approx ~init:G.Node.Map.empty
            ~f:(fun ~key:n ~data approx ->
                Map.add approx ~key:nodes.(n) ~data);
      } in
    let rec loop iters works approx =
      if can_iter iters then match step works approx with
        | Done approx -> make_solution iters approx
        | Step (works,approx) -> loop (iters+1) works approx
      else make_solution iters approx in
    let works = List.init (Array.length nodes) ident in
    loop 0 (Int.Set.of_list works) Int.Map.empty
end



module Reindex = struct
  module State = Monad.State.Make(Source)(Monad.Ident)
  open State.Syntax
  type 'a m = 'a Monad.State.T1(Source)(Monad.Ident).t


  let rec ids_of_trees trees =
    List.fold trees ~init:Id.Set.empty ~f:(fun xs t -> match t with
        | {data=Atom _; id} -> Set.add xs id
        | {data=List ts;id} ->
          Set.union (Set.add xs id) (ids_of_trees ts))

  let ids_of_defs defs f =
    List.map defs ~f |> ids_of_trees

  let macro_ids p = Id.Set.union_list [
      ids_of_defs p.macros Def.Macro.body;
      ids_of_defs p.consts Def.Const.value;
      ids_of_trees (List.concat_map p.substs ~f:Def.Subst.body);
    ]

  let derive tid =
    State.get () >>= fun src ->
    let nextid = Id.next (Source.lastid src) in
    State.put (Source.derived src ~from:tid nextid) >>| fun () ->
    nextid

  let reindex_def macros def =
    let rec map t : ast m =
      rename t >>= fun t -> match t.data with
      | Int _ | Var _ | Err _ -> State.return t
      | Ite (x,y,z) ->
        map x >>= fun x ->
        map y >>= fun y ->
        map z >>| fun z ->
        {t with data = Ite (x,y,z)}
      | Let (c,x,y) ->
        map x >>= fun x ->
        map y >>| fun y ->
        {t with data = Let (c,x,y)}
      | Rep (x,y) ->
        map x >>= fun x ->
        map y >>| fun ys ->
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
        map x >>| fun x ->
        {t with data = Set (v,x)}
    and map_all = State.List.map ~f:map
    and rename t =
      if Set.mem macros t.id || Id.null = t.id
      then derive t.id >>| fun id -> {t with id}
      else State.return t in
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

  let empty = Id.Map.empty
  let merge = union empty
  let bind id typ = Id.Map.singleton id typ
  let get gamma id = match Map.find gamma id with
    | None -> Tvar id
    | Some t -> t


  let make_join t t' = match t,t' with
    | Join (x,y), z when x = z || y = z -> t
    | _ -> if t = t' then t else Join (t,t')

  let make_meet t t' = match t,t' with
    | Join (x,y),z when x = z || y = z -> z
    | _ -> if t = t' then t else Meet (t,t')


  (** [subst gamma x y] substitute all occurences of [x] with [y] *)
  let subst gamma x y =
    let rec subst t = if t = x then y else match t with
        | Meet (t1,t2) -> make_meet (subst t1) (subst t2)
        | Join (t1,t2) -> make_join (subst t1) (subst t2)
        | grnd -> grnd in
    Map.map gamma ~f:subst

  (** [unify gamma x y r] substitute all occurences of type
      [x] and [y] with [r]  *)
  let unify gamma x y r =
    subst (subst gamma x r) y r

  let join id1 id2 id gamma =
    gamma ++ bind id (make_join (get gamma id1) (get gamma id2))

  let meet id1 id2 id gamma =
    match get gamma id1, get gamma id2 with
    | (Tvar x as t), (Tvar y as t') ->
      bind id (Tvar id) ++ unify gamma t t' (Tvar id)
    | (Tvar x as t), t'
    | t', (Tvar x as t) -> bind id t' ++ subst gamma t t'
    | t1,t2 -> bind id (make_meet t1 t2) ++ gamma

  let meet_with_ground g t n = match t with
    | Tvar x -> subst g t (Grnd n)
    | Meet (x,y) -> unify g x y (Grnd n)
    | Join (x,y) ->
      subst (subst g x (Grnd n)) y (Grnd n)
    | Grnd m -> if m = n then g
      else subst g t (make_meet t (Grnd n))


  let apply_signature ts g {args; rest; ret} =
    let rec apply g ts ns = match ts,ns with
      | ts,[] -> Some g,ts
      | [],_ -> None,[]
      | t :: ts, Any :: ns -> apply g ts ns
      | t :: ts, Type n :: ns ->
        apply (meet_with_ground g (get g t.id) n) ts ns in
    match apply g ts args with
    | None,_ -> None
    | Some g,[] -> Some g
    | Some g, ts -> match rest with
      | None -> None
      | Some Any -> Some g
      | Some (Type n) ->
        Some (List.fold ts ~init:g ~f:(fun g t ->
            meet_with_ground g (get g t.id) n))

  let type_of_expr gamma expr = match Map.find gamma expr.id with
    | Some (Grnd n) -> Type n
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

  let apply glob id name args gamma =
    signatures glob gamma name|>
    List.find_map ~f:(apply_signature args gamma) |> function
    | None -> gamma
    | Some gamma -> gamma

  let last xs = match List.rev xs with
    | {id} :: _ -> id
    | _ -> Id.null

  let constr id typ gamma = match typ with
    | Any -> Map.add gamma ~key:id ~data:(Tvar id)
    | Type n -> Map.add gamma ~key:id ~data:(Grnd n)

  let constr_glob {globs} vars var gamma =
    if Map.mem vars var.data.exp then gamma
    else match Map.find globs var.data.exp with
      | None -> gamma
      | Some n -> Map.add gamma ~key:var.id ~data:(Grnd n)

  let push vars {data; id} =
    Map.add vars ~key:data.exp ~data:id

  let varclass vars v = match Map.find vars v.data.exp with
    | None -> v.id
    | Some id -> id

  let (++) f g x = f (g x)

  type gamma = texpr Id.Map.t

  let infer_ast glob bindings ast : gamma -> gamma =
    let rec infer vs = function
      | {data=Int x; id} ->
        constr id x.data.typ
      | {data=Var v; id} ->
        meet v.id (varclass vs v) id ++
        constr_glob glob vs v ++
        constr v.id v.data.typ
      | {data=Ite (x,y,z); id} ->
        join y.id z.id id ++
        infer vs x ++
        infer vs y ++
        infer vs z
      | {data=Let (v,x,y); id} ->
        meet y.id id id ++
        infer (push vs v) y ++
        meet x.id v.id v.id ++
        infer vs x ++
        constr v.id v.data.typ
      | {data=App ((Dynamic name),xs); id} ->
        apply glob id name xs ++
        reduce vs xs
      | {data=Seq []; id} -> constr id Any
      | {data=Seq xs; id} ->
        meet (last xs) id id ++
        reduce vs xs
      | {data=Set (v,x); id} ->
        meet v.id id id ++
        meet v.id x.id (varclass vs v) ++
        constr v.id v.data.typ ++
        infer vs x
      | {data=Rep (c,x); id} ->
        meet c.id id id ++
        infer vs c ++
        infer vs x
      | {data=Msg (_,xs); id} ->
        constr id (Lisp.Type.word 1) ++
        reduce vs xs
      | {data=Err _; id} ->
        constr id Any
      | {data=App (Static _,_)} ->
        ident
    and reduce vs = function
      | [] -> ident
      | x :: xs -> infer vs x ++ reduce vs xs in
    infer bindings ast

  let find_func funcs id =
    List.find funcs ~f:(fun f -> f.id = id)

  let transfer glob node gamma = match node with
    | Entry | Exit -> gamma
    | Defun id -> match find_func glob.funcs id with
      | None -> gamma
      | Some f ->
        let args = Def.Func.args f in
        let vars = List.fold args ~init:String.Map.empty ~f:push in
        let gamma = List.fold args ~init:gamma ~f:(fun gamma v ->
            constr v.id v.data.typ gamma) in
        infer_ast glob vars (Def.Func.body f) gamma


  let unify_meets g =
    Map.fold g ~init:g ~f:(fun ~key:id ~data:t g ->
        match t with
        | Meet ((Tvar x as t), t')
        | Meet (t', (Tvar x as t)) -> subst g t t'
        | _ -> g)

  let meet g1 g2 =
    Map.merge g1 g2 ~f:(fun ~key -> function
        | `Left t | `Right t -> Some t
        | `Both (t,t') -> Some (make_meet t t')) |>
    unify_meets


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
    let init = Id.Map.empty in
    let fp =
      Fixpoint.compute (module Callgraph) ~rev:true ~start:Exit
        ~equal ~meet ~init ~f:(transfer glob) g in
    Fixpoint.solution fp Entry

  let rec well_typed = function
    | Meet (Grnd x, Grnd y) -> x = y
    | Meet (x,y) -> well_typed x && well_typed y
    | Join (x,y) -> well_typed x || well_typed y
    | Tvar _ | Grnd _ -> true


  (* The public interface  *)
  module Type = struct
    type error = Bot of Loc.t * texpr
    let rec pp_expr ppf = function
      | Grnd x -> fprintf ppf "%d" x
      | Meet (x,y) -> fprintf ppf "%a=%a" pp_expr x pp_expr y
      | Join (x,y) -> fprintf ppf "%a:%a" pp_expr x pp_expr y
      | Tvar _ -> fprintf ppf "?"

    let check vars p =
      let p = Reindex.program p in
      let gamma = infer vars p in
      eprintf "Inferred %d types@\n%!"
        (Map.length gamma);
      Map.iteri gamma ~f:(fun ~key ~data ->
          eprintf "%a: %a@\n%!"
            Loc.pp (Source.loc p.sources key) pp_expr data);
      Map.fold gamma ~init:[] ~f:(fun ~key:id ~data:t errs ->
          if well_typed t then errs
          else Bot (Source.loc p.sources id, t) :: errs)


    let pp_error ppf (Bot (loc,expr)) =
      fprintf ppf "%a@\nType error - expression is ill-typed: %a"
        Loc.pp loc pp_expr expr



    include Lisp.Type
  end
end

let pp_term pp_exp ppf = function
  | {data={exp; typ=Any}} -> fprintf ppf "%a" pp_exp exp
  | {data={exp; typ}} -> fprintf ppf "%a:%a" pp_exp exp Lisp.Type.pp typ

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

module Context = Lisp.Context
module Type = Typing.Type
