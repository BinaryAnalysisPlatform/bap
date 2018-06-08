open Core_kernel
open Bap.Std
open Bil.Types
open Graphlib.Std
open Regular.Std

type bid = Int63.t  [@@deriving bin_io, compare, sexp]

type value =
  | Enter
  | Atom of stmt
  | If_node of (exp * node)
  | While_node of (exp * bil)
  | Merge
  | Exit
and node = bid * value
[@@deriving bin_io, compare, sexp]

type edge = Goto | Fail | Take [@@deriving bin_io, compare, sexp]

module Bid = struct
  type t = bid [@@deriving bin_io, compare, sexp]

  let fresh =
    let x = ref (Int63.zero) in
    fun () ->
      let r = !x in
      Int63.incr x;
      r

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let pp fmt t =
        Format.fprintf fmt "%Ld" (Int63.to_int64 t)
      let module_name = Some "Bid"
      let hash = Hashtbl.hash
      let version = "1.0"
    end)
end

module Node = struct
  type t = node [@@deriving bin_io, compare, sexp]

  let make x = Bid.fresh (), x
  let atom  s  = make (Atom s)
  let enter () = make Enter
  let exit  () = make Exit
  let merge () = make Merge
  let if_ exp merge  = make (If_node (exp, merge))
  let while_ exp bil = make (While_node (exp,bil))
  let bid (x,_) = x
  let value (_,x) = x
  let with_value (id,_) x = id,x

  let is_enter (_,x) = match x with
    | Enter -> true
    | _ -> false

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let pp fmt (bid,t) =
        let s = match t with
          | Enter -> "enter"
          | Exit  -> "exit"
          | Merge -> "merge"
          | Atom s -> Stmt.to_string s
          | If_node (e, _) -> sprintf "if %s" (Exp.to_string e)
          | While_node (e,_) -> sprintf "while %s" (Exp.to_string e) in
        Format.fprintf fmt "%s: %s" (Bid.to_string bid) s
      let module_name = Some "Node"
      let hash = Hashtbl.hash
      let version = "1.0"
    end)
end

module Edge = struct
  type t = edge [@@deriving bin_io, compare, sexp]
  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let pp fmt t =
        Format.fprintf fmt "%s" (Sexp.to_string (sexp_of_t t))
      let module_name = Some "Edge"
      let hash = Hashtbl.hash
      let version = "1.0"
    end)

  let goto = Goto
  let fail = Fail
  let take = Take
  let is_fail = equal Fail
  let is_goto = equal Goto
  let is_take = equal Take
end

module G = Graphlib.Make(Node)(Edge)

let depth_first_search = Graphlib.depth_first_search (module G)

(* invariants:
   1. any condition node has both 'take' and 'fail' in output edges
   2. any 'if' condition ends with merge node
   3. any 'atom' node has one input edge and one output edge
   4. cfg contains only one 'enter' node with the only one output edge
   5. cfg contains at least on 'exit' node *)
let cfg_of_bil bil =
  let add cfg (predc, edge) node =
    let cfg = G.Node.insert node cfg in
    let e = G.Edge.create predc node edge in
    G.Edge.insert e cfg in
  let rec run cfg predc = function
    | [] -> cfg, predc
    | If (_, [],[]) :: bil -> run cfg predc bil
    | If (cond, yes, no) :: bil ->
      let merge = Node.merge () in
      let node = Node.if_ cond merge in
      let cfg = add cfg predc node in
      let cfg,predc  = run cfg (node, Edge.take) yes in
      let cfg,predc' = run cfg (node, Edge.fail) no in
      let cfg = add cfg predc merge in
      let cfg = add cfg predc' merge in
      run cfg (merge, Edge.goto) bil
    | While (cond, body) :: bil ->
      let node = Node.while_ cond body in
      let cfg = add cfg predc node in
      let cfg = match body with
        | [] -> add cfg (node, Edge.take) node
        | _ ->
          let cfg,(predc,_) = run cfg (node, Edge.take) body in
          add cfg (predc, Edge.goto) node in
      run cfg (node, Edge.fail) bil
    | s :: bil ->
      let node = Node.atom s in
      let cfg = add cfg predc node in
      run cfg (node, Edge.goto) bil in
  let cfg, predc = run G.empty (Node.enter (), Edge.goto) bil in
  add cfg predc (Node.exit ())

let enter g = Seq.find (G.nodes g) ~f:Node.is_enter
let find_edge edges f = List.find edges ~f:(fun e -> f (G.Edge.label e))
let find_edge_exn edges f = Option.value_exn (find_edge edges f)
let find_take_exn edges = find_edge_exn edges Edge.is_take
let find_fail_exn edges = find_edge_exn edges Edge.is_fail

let enter' g =
  Option.(enter g >>= fun n ->
          Seq.hd (G.Node.outputs n g) >>= fun e ->
          Some (n,e))

let bil_of_cfg ?remove cfg =
  let to_remove = match remove with
    | None -> fun _ -> false
    | Some s -> fun node -> Set.mem s node in
  let rec loop acc = function
    | [] -> List.rev acc, []
    | edge :: edges ->
      let dst = G.Edge.dst edge in
      let outputs = Seq.to_list (G.Node.outputs dst cfg) in
      if to_remove dst then
        loop acc (outputs @ edges)
      else
        match Node.value dst with
        | If_node (cond, _) ->
          let take = find_take_exn outputs in
          let fail = find_fail_exn outputs in
          let yes, next = loop [] [take] in
          let no,_ = loop [] [fail] in
          loop (If (cond, yes, no) :: acc) (next @ edges)
        | Merge | Exit -> List.rev acc, outputs
        | Enter  -> loop acc (outputs @ edges)
        | Atom s -> loop (s :: acc) (outputs @ edges)
        | While_node (cond, body) ->
          let fail = find_fail_exn outputs in
          loop (While (cond, body) :: acc) (fail :: edges) in
  match enter' cfg with
  | None -> []
  | Some (_,e) -> fst (loop [] [e])

(* simple worklist algorithm *)
module Const = struct

  type const = Defined of word | Undefined
  type input = const Var.Map.t
  type env = input G.Edge.Map.t

  module Input = struct
    type t = input

    let empty = Var.Map.empty
    let add = Map.add
    let find = Map.find
    let find_exn = Map.find_exn
    let is_empty = Map.is_empty

    let update t var = function
      | Int i -> Map.add t var (Defined i)
      | _ -> Map.add t var Undefined

    let merge ts =
      let both_defined = function
        | `Both (Defined w1, Defined w2) ->
          Option.some_if (Word.equal w1 w2) (Defined w1)
        | _ -> None in
      match ts with
      | [] -> empty
      | x :: xs ->
        List.fold xs ~init:x ~f:(Map.merge ~f:(fun ~key:_ -> both_defined))
  end

  module Env = struct
    type t = env
    let empty = G.Edge.Map.empty
    let update state env edge = Map.update env edge ~f:(fun _ -> state)
    let update' state env edges =
      List.fold edges ~init:env ~f:(update state)
    let find_exn = Map.find_exn
  end

  class apply input = object
    inherit Stmt.mapper as super
    method! map_var v =
      match Input.find input v with
      | Some (Defined i) -> Int i
      | _ -> Var v
    method! map_exp e = match e with
      | Let _ -> e
      | _ -> super#map_exp e |> Exp.fold_consts
  end

  let (@@) e input = (new apply input)#map_exp e
  let (@$) s input = (new apply input)#map_stmt s

  let update_state input = function
    | Move (v, e) -> Input.update input v (e @@ input)
    | _ -> input

  let defs bil =
    (object
      inherit [Var.Set.t] Stmt.visitor
      method! enter_move v _ defs = Set.add defs v
    end)#run bil Var.Set.empty

  let merge env inputs =
    Seq.filter_map inputs ~f:(fun inp ->
        let e = Env.find_exn env inp in
        Option.some_if (not (Input.is_empty e)) e) |>
    Seq.to_list |> Input.merge

  let run cfg =
    let rec loop env predc = function
      | [] -> env, predc
      | e :: worklist ->
        let target = G.Edge.dst e in
        let outputs = G.Node.outputs target cfg in
        match Node.value target with
        | Atom s ->
          let outputs = Seq.to_list outputs in
          let in_state = Env.find_exn env e in
          let out_state = update_state in_state s in
          let env = Env.update' out_state env outputs in
          loop env target (outputs @ worklist)
        | If_node (cond,_) ->
          let outputs = Seq.to_list outputs in
          let in_state = Env.find_exn env e in
          let fail = find_fail_exn outputs in
          let take = find_take_exn outputs in
          let env, merge_node = match cond @@ in_state with
            | Int w when Word.is_one w ->
              loop (Env.update in_state env take) target [take]
            | Int w when Word.is_zero w ->
              loop (Env.update in_state env fail) target [fail]
            | _ ->
              let env = Env.update' in_state env [take; fail] in
              loop env target [take; fail] in
          let outputs' = Seq.to_list (G.Node.outputs merge_node cfg) in
          let merged = merge env (G.Node.inputs merge_node cfg) in
          let env = Env.update' merged env outputs' in
          loop env target (outputs' @ worklist)
        | Merge -> loop env target worklist
        | While_node (_,body) ->
          let in_state = Env.find_exn env e in
          let state = Set.fold (defs body) ~init:in_state
              ~f:(fun state v -> Input.add state v Undefined) in
          let fail_edge = find_fail_exn (Seq.to_list outputs) in
          let env = Env.update state env fail_edge in
          loop env target (fail_edge :: worklist)
        | Enter | Exit -> env, predc in
    let init = Seq.fold (G.edges cfg)
        ~init:Env.empty ~f:(Env.update Input.empty) in
    match enter' cfg with
    | None -> init
    | Some (node, edge) -> fst (loop init node [edge])

  let find nodes bid =
    Seq.find nodes ~f:(fun n -> Bid.equal (Node.bid n) bid)

  let propagate bil =
    let cfg = cfg_of_bil bil in
    let env = run cfg in
    let find_input node =
      let inputs = G.Node.inputs node cfg in
      Env.find_exn env (Seq.hd_exn inputs) in
    let cfg' = Seq.fold (G.nodes cfg) ~init:G.empty
 	~f:(fun cfg' node ->
            let value = match Node.value node with
              | Atom s -> Atom (List.hd_exn (s @$ find_input node))
              | If_node (c,m) -> If_node ((c @@ find_input node), m)
 	      | x -> x in
            let node = Node.with_value node value in
 	    G.Node.insert node cfg') in
    let nodes = G.nodes cfg' in
    let find_same x = Option.value_exn (find nodes (Node.bid x)) in
    Seq.fold ~init:cfg' (G.edges cfg) ~f:(fun cfg' e ->
        let src = find_same (G.Edge.src e) in
        let dst = find_same (G.Edge.dst e) in
 	G.Edge.insert (G.Edge.create src dst (G.Edge.label e)) cfg')

  let is_def v = function
    | Move (v', _) -> Var.equal v v'
    | _ -> false

  let is_use s v = Set.mem (Stmt.free_vars s) v
  let is_use' e v = Set.mem (Exp.free_vars e) v

  let is_removable cfg var enter =
    let is_stop edge = function
      | None -> false
      | Some node -> Node.equal (G.Edge.dst edge) node in
    let rec loop ?stop = function
      | [] -> None, []
      | edge :: edges when is_stop edge stop ->
        let outs = Seq.to_list (G.Node.outputs (G.Edge.dst edge) cfg) in
        None, outs @ edges
      | edge :: edges ->
        let dst = G.Edge.dst edge in
        let out = Seq.to_list (G.Node.outputs dst cfg) in
        match Node.value dst with
        | Atom s ->
          if is_def var s then Some true, []
          else if is_use s var then Some false, []
          else loop ?stop (out @ edges)
        | If_node (c,merge) ->
          if is_use' c var then Some false, []
          else
            let take = find_take_exn out in
            let fail = find_fail_exn out in
            let r1,outs = loop ~stop:merge [take] in
            let r2,_ = loop ~stop:merge [fail] in
            begin
              match r1, r2 with
              | Some r1, Some r2 when Bool.equal r1 r2 -> Some r1, []
              | Some _, Some _ -> Some false, []
              | Some r1, None | None, Some r1 -> Some r1, []
              | None, None -> loop ?stop (outs @ edges)
            end
        | While_node (c,_) ->
          if is_use' c var then Some false, []
          else
            let take = find_take_exn out in
            let fail = find_fail_exn out in
            let (r,_) as res = loop ~stop:dst [take] in
            if Option.is_some r then res
            else
              loop (fail :: edges)
        | Merge -> loop ?stop (out @ edges)
        | Exit | Enter -> None, out in
    let start = Seq.hd_exn (G.Node.outputs enter cfg) in
    match fst (loop [start]) with
    | Some x -> x
    | None -> true

  let propagate_copy bil =
    let cfg = propagate bil in
    let nodes = G.nodes cfg in
    let consts = Seq.fold nodes ~init:Node.Set.empty
        ~f:(fun consts node ->
            match Node.value node with
            | Atom (Move (v, Int _)) ->
              if Var.is_physical v then consts
              else
              if is_removable cfg v node then
                Set.add consts node
              else consts
            | _ -> consts) in
    bil_of_cfg cfg ~remove:consts

end

let simpl_cond_stmts =
  let rec stmt = function
    | While (c,ss) -> while_ c ss
    | If (c,ts,fs) -> if_ c ts fs
    | s -> [s]
  and if_ c ts fs = match c with
    | Int x ->
      if Word.is_zero x then bil fs else bil ts
    | c -> [If (c, bil ts, bil fs)]
  and while_  c ss = match c with
    | Int x when Word.is_zero x -> []
    | c -> [While (c, bil ss)]
  and bil = List.concat_map ~f:stmt in
  bil

let propagate_consts bil =
  Const.propagate bil |> bil_of_cfg |> simpl_cond_stmts

let propagate_copy bil =
  Const.propagate_copy bil |> simpl_cond_stmts
