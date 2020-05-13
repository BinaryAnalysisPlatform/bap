open Core_kernel
open Bap.Std
open Regular.Std
open Graphlib.Std

type jmp_update = {
  cond : exp;
  kind : jmp_kind;
}

type update = Rhs of exp | Jmp of jmp_update

type t = {
  deads   : Tid.Set.t;
  updates : update Tid.Map.t;
}

let updated_term = Value.Tag.register (module Unit)
    ~name:"updated-term"
    ~uuid:"d21d76fa-12dd-470f-902e-f0e890e382d3"

let mark_updated t = Term.set_attr t updated_term ()
let is_updated t = Option.is_some (Term.get_attr t updated_term)

let drop_index = (object
  inherit Exp.mapper
  method! map_sym var = Var.base var
end)#map_exp

let updates_of_sub sub =
  let fold t cls init ~f = Seq.fold (Term.enum cls t) ~init ~f in
  let update_rhs updates d =
    let rhs = drop_index (Def.rhs d) in
    Map.set updates (Term.tid d) (Rhs rhs) in
  let update_jmp updates j =
    let j = Jmp.map_exp ~f:drop_index j in
    let data = Jmp {cond = Jmp.cond j; kind = Jmp.kind j} in
    Map.set updates (Term.tid j) data in
  let add_if add updates t =
    if is_updated t then add updates t else updates in
  fold sub blk_t Tid.Map.empty ~f:(fun updates b ->
      fold b def_t updates ~f:(add_if update_rhs) |>
      fold b jmp_t ~f:(add_if update_jmp))

let create ~deads sub = {deads; updates = updates_of_sub sub}

let (++) = Set.union

let dead_jmps_of_blk b =
  Term.to_sequence jmp_t b |>
  Seq.fold ~init:(Set.empty (module Tid), false)
    ~f:(fun (deads, is_unreachable) jmp ->
        if is_unreachable
        then Set.add deads (Term.tid jmp), is_unreachable
        else match Jmp.cond jmp with
          | Bil.Int x when x = Word.b1 -> deads, true
          | Bil.Int x when x = Word.b0 ->
            Set.add deads (Term.tid jmp), is_unreachable
          | _ -> deads, is_unreachable) |> fst

let dead_jmps sub =
  Term.to_sequence blk_t sub |>
  Seq.fold ~init:(Set.empty (module Tid))
    ~f:(fun tids b -> tids ++ dead_jmps_of_blk b)

let remove_dead_edges g dead_jmps =
  let module G = Graphs.Tid in
  Seq.fold (G.edges g)
    ~init:g ~f:(fun g edge ->
        if Set.mem dead_jmps (Graphs.Tid.Edge.label edge)
        then G.Edge.remove edge g
        else g)

let dead_blks g =
  let module G = Graphs.Tid in
  fst @@
  Graphlib.depth_first_search (module G) g
    ~init:(Set.empty (module Tid), false)
    ~start_tree:(fun node (deads, _) ->
        deads, Tid.equal node G.start)
    ~enter_node:(fun _ node (deads, is_reachable) ->
        if is_reachable then deads, is_reachable
        else Set.add deads node, is_reachable)

let find_unreachable sub t =
  let dead_jmps = dead_jmps sub in
  let dead_blks =
    remove_dead_edges (Sub.to_graph sub) dead_jmps |>
    dead_blks in
  {t with deads = t.deads ++ dead_jmps ++ dead_blks }

let update_def updates d =
  match Map.find updates (Term.tid d) with
  | None -> d
  | Some (Rhs e) -> Def.with_rhs d e
  | _ -> assert false

let update_jmp updates j =
  match Map.find updates (Term.tid j) with
  | None -> j
  | Some (Jmp {cond; kind}) ->
    let j = Jmp.with_cond j cond in
    Jmp.with_kind j kind
  | _ -> assert false

let update sub {updates} =
  Term.map blk_t sub ~f:(fun b ->
      Term.map def_t b ~f:(update_def updates) |>
      Term.map jmp_t ~f:(update_jmp updates))

let filter_map_alive deads cls ?(f=ident) x =
  Term.filter_map cls x ~f:(fun t ->
      if Set.mem deads (Term.tid t) then None
      else Some (f t))

let remove_dead_code sub {deads} =
  let update_blk b =
    filter_map_alive deads def_t b |>
    filter_map_alive deads jmp_t in
  filter_map_alive deads blk_t sub ~f:update_blk

let apply sub {deads; updates} =
  let update_blk b =
    filter_map_alive deads def_t b ~f:(update_def updates) |>
    filter_map_alive deads jmp_t ~f:(update_jmp updates) in
  filter_map_alive deads blk_t sub ~f:update_blk

include Data.Make(struct
    type nonrec t = t
    let version = "0.1"
  end)
