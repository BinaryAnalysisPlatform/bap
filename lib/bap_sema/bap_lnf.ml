open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_std

module Cfg = Block.Cfg
module Comp = Graph.Components.Make(Cfg)
module Basic_block = Block
module Block = Cfg.Block
module BS = Block.Set

type block  = Block.t with sexp_of
type blocks = BS.t with sexp_of

type tree = {
  hdrs: blocks;
  body: blocks;
  subs: tree list
} with sexp_of

type t = tree list with sexp_of

let trees = ident
let children t = t.subs
let headers t = t.hdrs
let bodies  t = t.body


let empty = {hdrs = BS.empty; body = BS.empty; subs = []}
let singleton x = {
  hdrs = BS.singleton x;
  body = BS.singleton x;
  subs = []
}

module Steensgard = struct
  let headers cfg scc =
    Cfg.fold_edges (fun src dst ss ->
        if BS.mem scc dst && not (BS.mem scc src)
        then BS.add ss dst
        else ss) cfg BS.empty
end

let rec of_cfg cfg = function
  | [] -> empty
  | [x] -> singleton x
  | scc ->
    let body = BS.of_list scc in
    let hdrs = Steensgard.headers cfg body in
    let cfg = Cfg.fold_edges_e (fun e cfg ->
        if BS.mem hdrs (Cfg.E.dst e) || BS.mem body (Cfg.E.src e)
        then Cfg.remove_edge_e cfg e else cfg) cfg cfg in
    let subs = Comp.scc_list cfg |> List.filter ~f:(fun scc ->
        List.for_all scc ~f:(BS.mem body) && match scc with
        | [x] -> Cfg.mem_edge cfg x x
        | _ -> true) |> List.map ~f:(of_cfg cfg) in
    {hdrs; body; subs}

let create ?bound entry =
  let _,cfg = Basic_block.to_graph ?bound entry in
  let sccs = List.filter (Comp.scc_list cfg) ~f:(function
      | [x] -> Cfg.mem_edge cfg x x
      | _ -> true)  in
  List.sort compare (List.map ~f:(of_cfg cfg) sccs)
