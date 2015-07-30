open Core_kernel.Std
open Bap_graph
open Bap_ir
open Bap_common
open Bap_bil
open Option.Monad_infix

module Jmp = Ir_jmp
module Blk = Ir_blk
module Sub = Ir_sub
module Seq = struct
  include Sequence
  include Bap_seq
end
type 'a seq = 'a Seq.t

module Pred = struct
  type t = Tid.Set.t Tid.Map.t with bin_io, compare, sexp
  (** [remove src dst preds] remove a predecessor [src] from
      a set of predecessors of [dst] *)
  let remove src dst rdep =
    Map.change rdep dst (function
        | None -> None
        | Some xs -> Some (Set.remove xs src))

  let remove_all src rdep =
    Map.map rdep ~f:(Set.filter ~f:(fun id -> id <> src))

  let update src dst rdep =
    Map.change rdep dst (function
        | None -> None
        | Some xs -> Some (Set.add xs src))

  let insert dst preds =
    Map.add preds ~key:dst ~data:Tid.Set.empty
end

(* internal representation of a graph:
   sub is the program itself
   preds is a mapping from a node to its predcessors,
   that is used to speed up backward processing.*)
type t = {
  preds : Pred.t;
  sub  : sub term;
} with bin_io, compare, sexp

type node = blk term with bin_io, compare, sexp

type edge = {
  src : blk term;
  dst : blk term;
  pos : int;
} with bin_io, sexp

let compare_edge x y = match Blk.compare x.src y.src with
  | 0 -> Blk.compare x.dst y.dst
  | n -> n

let empty_sub = Sub.create ()

(** extracts a successor's tid from a jump term  *)
let succ_tid_of_jmp jmp : tid option = match Jmp.kind jmp with
  | Goto (Direct tid) -> Some tid
  | Int (_,tid) -> Some tid
  | Call t -> Option.(Call.return t >>= function
    | Direct tid -> Some tid
    | _ -> None)
  | _ -> None

let succ_of_jmp sub jmp =
  match succ_tid_of_jmp jmp with
  | Some tid -> Term.find blk_t sub tid
  | _ -> None

let succs_of_blk blk =
  Term.enum jmp_t blk |> Seq.filter_map ~f:succ_tid_of_jmp

type difference_kind =
  | Target_change of tid * tid
  | New_jmp of tid
  | Del_jmp of tid
with variants,sexp

let jmp_set b = Term.enum jmp_t b |> Seq.map ~f:Term.tid |>
                Seq.fold ~init:Tid.Set.empty ~f:Set.add

(** [control_flow_difference bx by] computes a changes in control flow
    produced by blocks [bx] and [by]. Only changes, that affects graph
    toplogy are computed, i.e., change in a condition expression, or
    change of the order doesn't affect the topology.  If jump kind is
    changed form local (goto) to some non-local, then it will be
    removed from the set of edges. *)
let control_flow_difference bx by =
  let (jxs,jys) = (jmp_set bx, jmp_set by) in
  let news = Set.diff jys jxs in
  let dels = Set.diff jxs jys in
  let coms = Set.inter jxs jys in
  let to_changes kind b js =
    let succ j = Term.find_exn jmp_t b j |> succ_tid_of_jmp in
    Set.to_list js |> List.filter_map ~f:succ |> List.map ~f:kind in
  let dels = to_changes del_jmp bx dels in
  let news = to_changes new_jmp by news in
  Set.fold coms ~init:(news @ dels) ~f:(fun changes id ->
      let jx = Term.find_exn jmp_t bx id in
      let jy = Term.find_exn jmp_t by id in
      match succ_tid_of_jmp jx, succ_tid_of_jmp jy with
      | None,_ -> changes
      | _,None -> del_jmp id :: changes
      | Some tx, Some ty ->
        if Tid.(tx = ty) then changes
        else target_change tx ty :: changes)

let blocks_of_tids sub ts =
  Set.to_sequence ts |> Seq.map ~f:(Term.find_exn blk_t sub)

module Node = struct
  type label = node
  type graph = t
  type t = node
  type nonrec edge = edge

  let create = ident

  let label = ident

  let mem blk t = Map.mem t.preds (Term.tid blk)

  let succs blk t : node seq =
    Term.enum jmp_t blk |>
    Seq.filter_map ~f:(succ_of_jmp t.sub) |>
    Seq.filter ~f:(fun blk -> mem blk t)

  let preds blk t : node seq =
    match Map.find t.preds (Term.tid blk) with
    | None -> Seq.empty
    | Some ts -> blocks_of_tids t.sub ts

  let inputs dst t = match Map.find t.preds (Term.tid dst) with
    | None -> Seq.empty
    | Some ps ->
      blocks_of_tids t.sub ps |> Seq.concat_map ~f:(fun src ->
          Term.enum jmp_t src |> Seq.filter_mapi ~f:(fun pos jmp ->
              match succ_tid_of_jmp jmp with
              | None -> None
              | Some tid when tid <> Term.tid dst -> None
              | Some tid -> Some {src; pos; dst}))

  let outputs src t : edge seq =
    Term.enum jmp_t src |> Seq.filter_mapi ~f:(fun pos jmp ->
        succ_of_jmp t.sub jmp >>= fun dst ->
        if mem src t && mem dst t then Some {src; pos; dst} else None)

  let in_degree blk t = match Map.find t.preds (Term.tid blk) with
    | None -> 0
    | Some ts -> Set.length ts

  let out_degree blk t = Seq.length (outputs blk t)

  let degree ?dir blk g = match dir with
    | None -> in_degree blk g + out_degree blk g
    | Some `Out -> out_degree blk g
    | Some `In  -> in_degree blk g

  let update b blk' t =
    let bid = Term.tid b in
    match Term.find blk_t t.sub bid with
    | None -> t
    | _ when not (Term.same b blk') -> t
    | Some blk ->
      let sub = Term.update blk_t t.sub blk' in
      match control_flow_difference blk blk' with
      | [] -> {t with sub}
      | ds -> List.fold ~init:{t with sub} ds ~f:(fun t -> function
          | New_jmp dst -> {t with preds = Pred.update bid dst t.preds}
          | Del_jmp dst -> {t with preds = Pred.remove bid dst t.preds}
          | Target_change (tx,ty) ->
            t.preds |>
            Pred.remove bid tx |>
            Pred.update bid ty |> fun preds -> {t with preds})

  let update_preds sub preds =
    Term.enum blk_t sub  |> Seq.fold ~init:preds ~f:(fun preds blk ->
        succs_of_blk blk |> Seq.fold ~init:preds ~f:(fun preds dst ->
            Pred.update (Term.tid blk) dst preds))

  let do_insert blk t =
    let sub = if Sub.(t.sub = empty_sub)
      then Sub.create () else t.sub in
    let sub = Term.append blk_t sub blk in
    let preds = Pred.insert (Term.tid blk) t.preds in
    {preds = update_preds sub preds; sub}

  let insert blk t : graph =
    if mem blk t then t
    else do_insert blk t

  let remove blk t =
    let id = Term.tid blk in
    { sub = Term.remove blk_t t.sub id;
      preds = Map.remove t.preds id |> Pred.remove_all id}

  let edge src dst t : edge option =
    inputs dst t |> Seq.find ~f:(fun e -> Term.same e.src src)

  (* can be defined as [let mem t s d = find t s d <> None],
     but the following would be more efficient. *)
  let has_edge src dst t : bool =
    Map.find t.preds (Term.tid dst) |> function
    | None -> false
    | Some ps -> Set.mem ps (Term.tid src)

  include Regular.Make(struct
      type t = blk term with bin_io, sexp
      let compare x y = Term.(Tid.compare (tid x) (tid y))
      let pp = Blk.pp
      let hash x = Tid.hash (Term.tid x)
      let module_name = Some "Bap.Std.Graphlib.Ir.Node"
    end)

end

module Edge = struct
  type label = int
  type nonrec node = node
  type graph = t
  type t = edge with compare

  let null = Exp.Int Bitvector.b0
  let dummy = Jmp.create_goto ~cond:null (Label.indirect null)

  let insert_jmp src pos dst =
    let n = Term.length jmp_t src - pos in
    let src = Fn.apply_n_times ~n
        (fun b -> Term.append jmp_t b dummy) src in
    let jmp = Jmp.create_goto (Label.direct (Term.tid dst)) in
    {src = Term.append jmp_t src jmp; dst; pos}

  let create src dst pos : edge =
    let tid = Term.tid dst in
    match Term.nth jmp_t src pos with
    | None -> insert_jmp src pos dst
    | Some jmp -> match succ_tid_of_jmp jmp with
      | Some id when Tid.equal id tid -> {src; dst; pos}
      | _ ->
        let src = Term.change jmp_t src (Term.tid jmp) (fun _ ->
            Some (Jmp.create_goto (Label.direct tid))) in
        {src; dst; pos}

  let label e : label = e.pos
  let src e = e.src
  let dst e = e.dst

  let mem e g = match Map.find g.preds (Term.tid e.dst) with
    | None -> false
    | Some ps -> Set.mem ps (Term.tid e.src)

  let jmps_before e src =
    Seq.take (Term.enum jmp_t src) e.pos

  let jmps_after e src =
    Seq.drop (Term.enum jmp_t src) (e.pos + 1)

  let jmps dir e g =
    match Term.find blk_t g.sub (Term.tid e.src) with
    | None -> Seq.empty
    | Some src -> match dir with
      | `after -> jmps_after e src
      | `before -> jmps_before e src

  let edges dir e g =
    jmps dir e g |> Seq.filter_mapi ~f:(fun pos jmp ->
        match succ_tid_of_jmp jmp with
        | None -> None
        | Some _ -> Some {e with pos})

  let jmp e = Term.nth_exn jmp_t e.src e.pos

  let tid e = Term.tid (jmp e)

  let simpl = Bap_helpers.Exp.(fixpoint fold_consts)

  let cond e g =
    jmps `before e g |>
    Seq.fold ~init:(Jmp.cond (jmp e)) ~f:(fun cond jmp ->
        let c = Exp.UnOp (Unop.NOT, Jmp.cond jmp) in
        Exp.BinOp (Binop.AND,cond,c)) |> simpl

  let insert e t = Node.(insert e.dst t |> insert e.src)

  let do_update e l t =
    let e = create e.src e.dst l in
    Node.(insert e.dst t |>
          insert e.src |>
          update e.src e.src |>
          update e.dst e.dst)

  let update e l t =
    if mem e t then do_update e l t else t

  let cut_tail pos blk =
    let b = Blk.Builder.init ~copy_phis:true ~copy_defs:true blk in
    Term.enum jmp_t blk |> Seq.iteri ~f:(fun i jmp ->
        if i < pos then Blk.Builder.add_jmp b jmp);
    Blk.Builder.result b

  let make_dummy jmp blk =
    Term.change jmp_t blk (Term.tid jmp) (fun _ -> Some dummy)

  let tail_length blk =
    with_return (fun {return} ->
        Term.enum ~rev:true jmp_t blk |>
        Seq.fold ~init:0 ~f:(fun len jmp ->
            if Term.same jmp dummy then (len + 1)
            else return len))

  let remove e t =
    match Term.find blk_t t.sub (Term.tid e.src) with
    | None -> t
    | Some src ->
      let len = Term.length jmp_t src in
      let src = match Term.nth jmp_t src e.pos with
        | None -> src
        | Some jmp -> make_dummy jmp src in
      let n = tail_length src in
      let src = if n = 0 then src else cut_tail (len - n) src in
      Node.update src src t

  include Regular.Make(struct
      type t = edge with bin_io, compare, sexp
      let module_name = Some "Bap.Std.Graphlib.Ir.Edge"
      let hash t = Blk.hash t.src lxor Blk.hash t.dst
      let pp ppf x =
        Option.iter (Term.nth jmp_t x.src x.pos) ~f:(Jmp.pp ppf)
    end)
end

let empty = {
  sub = Sub.create ();
  preds = Tid.Map.empty;
}

let create ?tid ?name () = {
  empty with
  sub = Sub.create ?tid ?name ();
}

let nodes t = Term.enum blk_t t.sub

let edges t =
  nodes t |> Seq.concat_map ~f:(fun src -> Node.outputs src t)

let number_of_edges t =
  Seq.(Map.to_sequence t.preds >>| snd |>
       sum (module Int) ~f:Set.length)


let number_of_nodes t = Term.length blk_t t.sub

let preds_of_sub sub : Tid.Set.t Tid.Map.t =
  Term.enum blk_t sub |>
  Seq.fold ~init:Tid.Map.empty ~f:(fun ins src ->
      let src_id = Term.tid src in
      let ins = Map.change ins src_id (function
          | None -> Some Tid.Set.empty
          | other -> other) in
      Term.enum jmp_t src |>
      Seq.fold ~init:ins ~f:(fun ins jmp ->
          match succ_tid_of_jmp jmp with
          | None -> ins
          | Some tid -> Map.change ins tid (function
              | None -> Some (Tid.Set.singleton src_id)
              | Some set -> Some (Set.add set src_id))))

let of_sub sub = {
  preds = preds_of_sub sub;
  sub;
}

let to_sub t = t.sub

let compare x y = Sub.compare x.sub y.sub

let is_directed = true

let create_tid_graph sub =
  let module G = Bap_graph_regular.Tid.Tid in
  Term.enum blk_t sub |> Seq.fold ~init:G.empty ~f:(fun g src ->
      let sid = Term.tid src in
      Term.enum jmp_t src |> Seq.fold ~init:g ~f:(fun g jmp ->
          match succ_tid_of_jmp jmp with
          | None -> g
          | Some did ->
            let jid = Term.tid jmp in
            let edge = G.Edge.create sid did jid in
            G.Edge.insert edge g))

include Regular.Make(struct
    type nonrec t = t with bin_io, compare, sexp
    let module_name = Some "Bap.Std.Graphlib.Ir"
    let hash g = Sub.hash g.sub
    let pp ppf g =
      let open Bap_graph_pp in
      let node_label blk =
        let open Bap_ir in
        let phis =
          Term.enum phi_t blk |> Seq.map ~f:Ir_phi.to_string in
        let defs =
          Term.enum def_t blk |> Seq.map ~f:Ir_def.to_string in
        let jmps =
          Term.enum jmp_t blk |> Seq.filter_map ~f:(fun jmp ->
              match Jmp.kind jmp with
              | Call _ | Ret _ | Int (_,_) -> Some (Jmp.to_string jmp)
              | Goto _ -> match succ_tid_of_jmp jmp with
                | None -> Some (Jmp.to_string jmp)
                | Some _ -> None) in
        let lines =
          List.concat @@ List.map [phis; defs; jmps] ~f:Seq.to_list in
        let body = String.concat lines |> String.concat_map
                     ~f:(function '\n' -> "\\l"
                                | c -> Char.to_string c) in
        sprintf "%s\n%s" (Term.name blk) body in
      let string_of_node b = sprintf "%s" (Term.name b) in
      let edge_label e = match Edge.cond e g with
        | Exp.Int w when Bitvector.is_one w -> ""
        | exp -> Bap_exp.to_string exp  in
      let nodes_of_edge e = Edge.(src e, dst e) in
      Dot.pp_graph
        ~attrs:["node[shape=box]"]
        ~string_of_node ~node_label ~edge_label
        ~nodes_of_edge ~nodes:(nodes g) ~edges:(edges g) ppf
  end)

let pp_blk ppf blk = Format.fprintf ppf "%a" Tid.pp (Term.tid blk)

module Tree = struct
  type t = blk term tree
  include Printable(struct
      type nonrec t = t
      let pp = Tree.pp pp_blk
      let module_name = Some "Bap.Std.Graphlib.Ir.Tree"
    end)
end

module Frontier = struct
  type t = blk term frontier
  include Printable(struct
      type nonrec t = t
      let pp = Frontier.pp pp_blk
      let module_name = Some "Bap.Std.Graphlib.Ir.Frontier"
    end)
end


module Group = struct
  type t = blk term group
  include Printable(struct
      type nonrec t = t
      let pp = Group.pp pp_blk
      let module_name = Some "Bap.Std.Graphlib.Ir.Group"
    end)
end

module Partition = struct
  type t = blk term partition
  include Printable(struct
      type nonrec t = t
      let pp = Partition.pp pp_blk
      let module_name = Some "Bap.Std.Graphlib.Ir.Partition"
    end)
end

module Path = struct
  type t = node path
  include Printable(struct
      type nonrec t = t
      let pp = Path.pp pp_blk
      let module_name = Some "Bap.Std.Graphlib.Ir.Path"
    end)
end
