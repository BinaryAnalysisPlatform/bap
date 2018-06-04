open Core_kernel
open Bap_common
open Bap_visitor
open Bap_stmt.Stmt
open Bap_bil
open Bap_bil.Stmt
open Bap_bil.Exp
open Graphlib.Std
open Regular.Std

module Word = Bitvector
module Var = Bap_var

type bid = Int63.t  [@@deriving bin_io, compare, sexp]

type value =
  | Enter
  | Atom of stmt
  | If_node of exp
  | While_node of (exp * bil)
  | Merge
[@@deriving bin_io, compare, sexp]

type node = bid * value [@@deriving bin_io, compare, sexp]
type edge = Goto | Fail | Take [@@deriving bin_io, compare, sexp]

module Bid = struct
  type t = bid [@@deriving bin_io, compare, sexp]

  let get_fresh =
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

  let make ?bid x = match bid with
    | None -> Bid.get_fresh (), x
    | Some bid -> bid, x

  let atom ?bid s = make ?bid (Atom s)
  let if_ ?bid exp = make ?bid (If_node exp)
  let while_ ?bid exp bil = make ?bid (While_node (exp,bil))
  let enter ?bid () = make ?bid Enter
  let merge ?bid () = make ?bid Merge
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
          | Merge -> "merge"
          | Atom s -> Bap_stmt.to_string s
          | If_node e -> sprintf "if %s" (Bap_exp.to_string e)
          | While_node (e,_) -> sprintf "while %s" (Bap_exp.to_string e) in
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

(* invariant: every condition node must have both take and fail
              in output edges
   invariant: merge node must be in the end of both take and
              fail edges and must be the same for both of them *)
let cfg_of_bil bil =
  let add cfg preds node =
    let cfg = G.Node.insert node cfg in
    List.fold preds ~init:cfg
      ~f:(fun cfg (src, edge) ->
          let e = G.Edge.create src node edge in
          G.Edge.insert e cfg) in
  let rec run cfg preds = function
    | [] -> cfg,preds
    | If (_, [],[]) :: bil -> run cfg preds bil
    | If (cond, yes, no) :: bil ->
      let node = Node.if_ cond in
      let cfg = add cfg preds node in
      let cfg,preds  = run cfg [node, Edge.take] yes in
      let cfg,preds' = run cfg [node, Edge.fail] no in
      let merge = Node.merge () in
      let cfg = add cfg (preds' @ preds) merge in
      run cfg [merge, Edge.goto] bil
    | While (cond, body) :: bil ->
      let node = Node.while_ cond body in
      let cfg = add cfg preds node in
      let cfg,preds = run cfg [node, Edge.take] body in
      let cfg = match preds with
        | [] -> cfg
        | (hd,_) :: _ when List.is_empty body ->
          add cfg [hd, Edge.take] node
        | (hd,_) :: _ -> add cfg [hd, Edge.goto] node in
      run cfg [node, Edge.fail] bil
    | s :: bil ->
      let node = Node.atom s in
      let cfg = add cfg preds node in
      run cfg [node, Edge.goto] bil in
  fst (run G.empty [Node.enter (), Edge.goto] bil)

let enter g = Seq.find (G.nodes g) ~f:Node.is_enter
let find_edge edges f = List.find edges ~f:(fun e -> f (G.Edge.label e))
let find_edge_exn edges f = Option.value_exn (find_edge edges f)
let find_take_exn edges = find_edge_exn edges Edge.is_take
let find_fail_exn edges = find_edge_exn edges Edge.is_fail

(* simple worklist algorithm *)
module Const = struct

  type const = Defined of word | Undefined
  type vector = const Var.Map.t
  type env = vector G.Edge.Map.t

  class apply input = object
    inherit bil_mapper as super
    method! map_var v =
      match Map.find input v with
      | Some (Defined i) -> Int i
      | _ -> Var v
    method! map_exp e = match e with
      | Let _ -> e
      | _ -> super#map_exp e |> Bap_helpers.Exp.fold_consts
  end

  let (@@) e input = (new apply input)#map_exp e
  let (@$) s input = (new apply input)#map_stmt s

  let update input var = function
    | Int i -> Map.add input var (Defined i)
    | _ -> Map.add input var Undefined

  let update_state input = function
    | Move (v, e) -> update input v (e @@ input)
    | _ -> input

  let merge inputs =
    let both_defined = function
      | `Both (Defined w1, Defined w2) ->
        Option.some_if (Word.equal w1 w2) (Defined w1)
      | _ -> None in
    match inputs with
    | [] -> Var.Map.empty
    | x :: xs ->
      List.fold xs ~init:x ~f:(Map.merge ~f:(fun ~key:_ -> both_defined))

  let defs bil =
    (object
      inherit [Var.Set.t] bil_visitor
      method! enter_move v _ defs =
        Set.add defs v
    end)#run bil Var.Set.empty

  let run cfg =
    let update_env state env edge =
      Map.update env edge ~f:(fun _ -> state) in
    let update_env' state env edges =
      List.fold edges ~init:env ~f:(update_env state) in
    let process_merge inputs outputs env =
      let out_state =
        Seq.filter_map inputs ~f:(fun inp ->
            let e = Map.find_exn env inp in
            Option.some_if (not (Map.is_empty e)) e) |>
        Seq.to_list |> merge in
      update_env' out_state env outputs in
    let rec loop env predc = function
      | [] -> env, predc
      | e :: worklist ->
        let target = G.Edge.dst e in
        let outputs = Seq.to_list (G.Node.outputs target cfg) in
        let in_state = Map.find_exn env e in
        match Node.value target with
        | Atom s ->
          let out_state = update_state in_state s in
          let env = update_env' out_state env outputs in
          loop env target (outputs @ worklist)
        | If_node cond ->
          let cond = cond @@ in_state in
          let fail = find_fail_exn outputs in
          let take = find_take_exn outputs in
          let env, merge = match cond @@ in_state with
            | Int w when Word.is_one w ->
              loop (update_env in_state env take) target [take]
            | Int w when Word.is_zero w ->
              loop (update_env in_state env fail) target [fail]
            | _ ->
              let env = update_env' in_state env [take; fail] in
              loop env target [take; fail] in
          let inputs = G.Node.inputs merge cfg in
          let outputs = Seq.to_list (G.Node.outputs merge cfg) in
          let env = process_merge inputs outputs env in
          loop env target (outputs @ worklist)
        | Merge -> loop env target worklist
        | While_node (_,body) ->
          let defs = defs body in
          let state = Set.fold defs ~init:in_state
              ~f:(fun state v -> Map.add state v Undefined) in
          let fail_edge = find_fail_exn outputs in
          let env = update_env state env fail_edge in
          loop env target (fail_edge :: worklist)
        | Enter -> env, predc in
    let init = Seq.fold (G.edges cfg)
        ~init:G.Edge.Map.empty ~f:(update_env Var.Map.empty) in
    Option.(enter cfg >>= fun node ->
            Seq.hd (G.Node.outputs node cfg) >>= fun edge ->
            Some (node, edge)) |> function
    | None -> init
    | Some (node, edge) -> fst (loop init node [edge])

  let find nodes bid =
    Seq.find nodes ~f:(fun n -> Bid.equal (Node.bid n) bid)

  let propagate bil =
    let cfg = cfg_of_bil bil in
    let env = run cfg in
    let find_input node =
      let inputs = G.Node.inputs node cfg in
      Map.find_exn env (Seq.hd_exn inputs) in
    let eval_node node =
      match Node.value node with
      | Atom s -> Atom (List.hd_exn (s @$ find_input node))
      | If_node c -> If_node (c @@ find_input node)
      | While_node (c,body) ->
        begin
          match c @@ find_input node with
          | Int w as e when Word.is_zero w -> While_node (e,body)
          | _ -> While_node (c,body)
        end
      | x -> x in
    let rec loop acc = function
      | [] -> List.rev acc, []
      | edge :: _ ->
        let dst = G.Edge.dst edge in
        let outputs = Seq.to_list (G.Node.outputs dst cfg) in
        match eval_node dst with
        | If_node cond ->
          let take = find_take_exn outputs in
          let fail = find_fail_exn outputs in
          let yes, next = loop [] [take] in
          let no,_ = loop [] [fail] in
          loop (If (cond, yes, no) :: acc) next
        | Merge  -> List.rev acc, outputs
        | Enter  -> loop acc outputs
        | Atom s -> loop (s :: acc) outputs
        | While_node (cond, body) ->
          let fail = find_fail_exn outputs in
          loop (While (cond, body) :: acc) [fail] in
    Option.(enter cfg >>= fun n -> Seq.hd (G.Node.outputs n cfg)) |> function
    | None -> []
    | Some e -> fst (loop [] [e])

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
  Const.propagate bil |> simpl_cond_stmts
