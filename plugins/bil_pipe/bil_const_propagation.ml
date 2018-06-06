open Core_kernel
open Bap.Std
open Bil.Types
open Graphlib.Std
open Regular.Std

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

  let make x = Bid.get_fresh (), x
  let atom s = make (Atom s)
  let if_ exp = make (If_node exp)
  let while_ exp bil = make (While_node (exp,bil))
  let enter () = make Enter
  let merge () = make Merge
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
          | Atom s -> Stmt.to_string s
          | If_node e -> sprintf "if %s" (Exp.to_string e)
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

(* invariant: every condition node has both take and fail
              in output edges
   invariant: any 'if' condition ends with merge node *)
let cfg_of_bil bil =
  let add cfg (predc, edge) node =
    let cfg = G.Node.insert node cfg in
    let e = G.Edge.create predc node edge in
    G.Edge.insert e cfg in
  let rec run cfg predc = function
    | [] -> cfg, predc
    | If (_, [],[]) :: bil -> run cfg predc bil
    | If (cond, yes, no) :: bil ->
      let node = Node.if_ cond in
      let cfg = add cfg predc node in
      let cfg,predc  = run cfg (node, Edge.take) yes in
      let cfg,predc' = run cfg (node, Edge.fail) no in
      let merge = Node.merge () in
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
  fst (run G.empty (Node.enter (), Edge.goto) bil)

let enter g = Seq.find (G.nodes g) ~f:Node.is_enter
let find_edge edges f = List.find edges ~f:(fun e -> f (G.Edge.label e))
let find_edge_exn edges f = Option.value_exn (find_edge edges f)
let find_take_exn edges = find_edge_exn edges Edge.is_take
let find_fail_exn edges = find_edge_exn edges Edge.is_fail

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
      method! enter_move v _ defs =
        Set.add defs v
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
        | If_node cond ->
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
        | Enter -> env, predc in
    let init = Seq.fold (G.edges cfg)
        ~init:Env.empty ~f:(Env.update Input.empty) in
    Option.(enter cfg >>= fun node ->
            Seq.hd (G.Node.outputs node cfg) >>= fun edge ->
            Some (node, edge)) |> function
    | None -> init
    | Some (node, edge) -> fst (loop init node [edge])

  let propagate bil =
    let cfg = cfg_of_bil bil in
    let env = run cfg in
    let find_input node =
      let inputs = G.Node.inputs node cfg in
      Env.find_exn env (Seq.hd_exn inputs) in
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
          loop (While (cond, body) :: acc) [find_fail_exn outputs] in
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

let propagate_copy bil =
  let rec loop acc = function
    | [] -> List.rev acc
    | s :: bil ->
      let acc =
        match s with
        | Move (v, Int _) as s ->
          if Var.is_virtual v then acc
          else s :: acc
        | If (cond, yes, no) -> If (cond,loop [] yes,loop [] no) :: acc
        | While (cond, body) -> While (cond, loop [] body) :: acc
        | s -> s :: acc in
      loop acc bil in
  let bil = propagate_consts bil in
  loop [] bil
