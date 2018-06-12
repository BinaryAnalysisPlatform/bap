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

(* invariants:
   1. any condition node has both 'take' and 'fail' in output edges
   2. there is a 'merge' node for any 'if' node, but not necessarily
      reachable from both edges
   3. any 'atom' node has a single input edge and (optionaly) a single output edge
   4. cfg contains only one 'enter' node with the only one output
      edge; consequently any bil statement has at least one input edge *)
let cfg_of_bil bil =
  let add cfg predc node =
    let cfg = G.Node.insert node cfg in
    match predc with
    | None -> cfg
    | Some (predc, edge) ->
      let e = G.Edge.create predc node edge in
      G.Edge.insert e cfg in
  let rec run cfg ?predc = function
    | [] -> cfg, predc
    | If (_, [],[]) :: bil -> run cfg ?predc bil
    | If (cond, yes, no) :: bil ->
      let merge = Node.merge () in
      let node = Node.if_ cond merge in
      let cfg = add cfg predc node in
      let cfg,last  = run cfg ~predc:(node, Edge.take) yes in
      let cfg,last' = run cfg ~predc:(node, Edge.fail) no in
      let cfg = add cfg last merge in
      let cfg = add cfg last' merge in
      run cfg ~predc:(merge, Edge.goto) bil
    | While (cond, body) :: bil ->
      let node = Node.while_ cond body in
      let cfg = add cfg predc node in
      let cfg = match body with
        | [] -> add cfg (Some (node, Edge.take)) node
        | _ ->
          let cfg,predc = run cfg ~predc:(node, Edge.take) body in
          match predc with
          | None -> cfg
          | Some (predc,_) ->
            add cfg (Some (predc, Edge.goto)) node in
      run cfg ~predc:(node, Edge.fail) bil
    | Jmp _ as s :: _ -> add cfg predc (Node.atom s), None
    | s :: bil ->
      let node = Node.atom s in
      let cfg = add cfg predc node in
      run cfg ~predc:(node, Edge.goto) bil in
  let cfg, _ = run G.empty ~predc:(Node.enter (), Edge.goto) bil in
  cfg

let edges_of_opt e = Option.value_map e ~default:[] ~f:(fun e -> [e])
let enter g = Seq.find (G.nodes g) ~f:Node.is_enter
let find_edge edges f = List.find edges ~f:(fun e -> f (G.Edge.label e))
let find_take edges = edges_of_opt (find_edge edges Edge.is_take)
let find_fail edges = edges_of_opt (find_edge edges Edge.is_fail)

let bil_of_cfg ?remove cfg =
  let mem = match remove with
    | None -> fun _ -> false
    | Some s -> fun node -> Set.mem s node in
  let rec loop acc = function
    | [] -> List.rev acc
    | edge :: edges ->
      let dst = G.Edge.dst edge in
      let outputs = Seq.to_list (G.Node.outputs dst cfg) in
      if mem dst then loop acc (outputs @ edges)
      else
        match Node.value dst with
        | If_node (cond,merge) ->
          let next = Seq.to_list (G.Node.outputs merge cfg) in
          let bil = match cond with
            | Int w when Word.is_one w -> loop [] (find_take outputs)
            | Int _ -> loop [] (find_fail outputs)
            | _ ->
              let yes = loop [] (find_take outputs) in
              let no  = loop [] (find_fail outputs) in
              [If (cond, yes, no)] in
          loop (List.rev bil @ acc) (next @ edges)
        | Merge -> List.rev acc
        | Enter  -> loop acc (outputs @ edges)
        | Atom s -> loop (s :: acc) (outputs @ edges)
        | While_node (cond, body) ->
          let acc' = match cond with
            | Int w when Word.is_zero w -> acc
            | _ -> While (cond, body) :: acc in
          loop acc' (find_fail outputs @ edges) in
  Option.(enter cfg >>= fun n -> Seq.hd (G.Node.outputs n cfg)) |> function
  | None -> []
  | Some e -> loop [] [e]

module Const = struct

  type const = Defined of word | Undefined
  type input = const Var.Map.t
  type env = input G.Edge.Map.t

  module Input = struct
    type t = input

    let empty = Var.Map.empty
    let add = Map.add
    let find = Map.find
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
    let find_exn = Map.find_exn
    let update state env edge = Map.update env edge ~f:(fun _ -> state)
    let update' state env edges =
      List.fold edges ~init:env ~f:(update state)
    let init cfg =
      Seq.fold (G.edges cfg) ~init:empty ~f:(update Input.empty)
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

  (* post condition: env contains entry for every edge of cfg,
     i.e. Env.find_exn is safe *)
  let run cfg =
    let rec loop env = function
      | [] -> env
      | e :: worklist ->
        let target = G.Edge.dst e in
        let outputs = Seq.to_list (G.Node.outputs target cfg) in
        let in_state = Env.find_exn env e in
        match Node.value target with
        | Atom s ->
          let out_state = update_state in_state s in
          let env = Env.update' out_state env outputs in
          loop env (outputs @ worklist)
        | If_node (cond,merge_node) ->
          let cond = cond @@ in_state in
          let fail = find_fail outputs in
          let take = find_take outputs in
          let env = match cond @@ in_state with
            | Bil.Int w when Word.is_one w  ->
              let env = Env.update' in_state env take in
              loop env take
            | Bil.Int w when Word.is_zero w ->
              let env = Env.update' in_state env fail in
              loop env fail
            | _ ->
              let env = Env.update' in_state env (take @ fail) in
              loop env (take @ fail) in
          let outputs' = Seq.to_list (G.Node.outputs merge_node cfg) in
          let merged = merge env (G.Node.inputs merge_node cfg) in
          let env = Env.update' merged env outputs' in
          loop env (outputs' @ worklist)
        | Merge -> loop env worklist
        | While_node (_,body) ->
          let defs = defs body in
          let state = Set.fold defs ~init:in_state
              ~f:(fun state v -> Input.add state v Undefined) in
          let fail = find_fail outputs in
          let env = Env.update' state env fail in
          let worklist = fail @ worklist in
          loop env worklist
        | Enter  -> loop env worklist in
    let env = Env.init cfg in
    match enter cfg with
    | None -> env
    | Some enter -> loop env (Seq.to_list (G.Node.outputs enter cfg))

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
            let node' = Node.with_value node
                (match Node.value node with
                 | Atom s -> Atom (List.hd_exn (s @$ find_input node))
                 | If_node (c,m) -> If_node ((c @@ find_input node), m)
 	         | x -> x) in
            G.Node.insert node' cfg') in
    let nodes = G.nodes cfg' in
    let find_same x = Option.value_exn (find nodes (Node.bid x)) in
    Seq.fold ~init:cfg' (G.edges cfg) ~f:(fun cfg' e ->
        let src = find_same (G.Edge.src e) in
        let dst = find_same (G.Edge.dst e) in
 	G.Edge.insert (G.Edge.create src dst (G.Edge.label e)) cfg')

  let propagate_consts bil = propagate bil |> bil_of_cfg

  let is_def v = function
    | Move (v', _) -> Var.equal v v'
    | _ -> false

  let is_use s v = Set.mem (Stmt.free_vars s) v
  let is_use' e v = Set.mem (Exp.free_vars e) v

  let is_removable cfg enter var =
    let is_stop edge = function
      | None -> false
      | Some node -> Node.equal (G.Edge.dst edge) node in
    let rec loop ?stop = function
      | [] -> None
      | edge :: _ when is_stop edge stop -> None
      | edge :: edges ->
        let dst = G.Edge.dst edge in
        let out = Seq.to_list (G.Node.outputs dst cfg) in
        match Node.value dst with
        | Atom s ->
          if is_use s var then Some false
          else if is_def var s then Some true
          else loop ?stop (out @ edges)
        | If_node (c,merge) ->
          if is_use' c var then Some false
          else
            let r1 = loop ~stop:merge (find_take out) in
            let r2 = loop ~stop:merge (find_fail out) in
            begin
              match r1, r2 with
              | Some r1, Some r2 when Bool.equal r1 r2 -> Some r1
              | Some _, Some _
              | Some _, None | None, Some _ -> Some false
              | None, None ->
                let outs = Seq.to_list (G.Node.outputs merge cfg) in
                loop ?stop (outs @ edges)
            end
        | While_node (c,_) ->
          if is_use' c var then Some false
          else
            let r = loop ~stop:dst (find_take out) in
            if Option.is_some r then r
            else loop ?stop (find_fail out @ edges)
        | Enter | Merge -> loop ?stop (out @ edges) in
    let outs = Seq.to_list (G.Node.outputs enter cfg) in
    Option.value ~default:true (loop outs)

  let propagate_copy bil =
    let cfg = propagate bil in
    let nodes = G.nodes cfg in
    let consts = Seq.fold nodes ~init:Node.Set.empty
        ~f:(fun consts node ->
            match Node.value node with
            | Atom (Move (v, _)) when Var.is_physical v -> consts
            | Atom (Move (v, Int _)) when is_removable cfg node v ->
              Set.add consts node
            | _ -> consts) in
    bil_of_cfg cfg ~remove:consts

end

let propagate_consts = Const.propagate_consts
let propagate_copy = Const.propagate_copy
