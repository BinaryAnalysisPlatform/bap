open Core_kernel.Std
open Bap_types.Std
open Graphlib.Std
open Bap_image_std

module Block = Bap_disasm_block
module Insn = Bap_disasm_insn
module Symtab = Bap_disasm_symtab
module Source = Bap_disasm_source
module Cfg = Bap_disasm_rec.Cfg
type block = Block.t
type symtab = Symtab.t

type cfg = Cfg.t

type t = Reconstructor of (cfg -> symtab)
type reconstructor = t

let create f = Reconstructor f
let run (Reconstructor f) = f

let roots_of_blk roots cfg blk =
  let addr = Block.addr blk in
  let term = Block.terminator blk in
  let init =
    if Set.mem roots addr || Seq.is_empty (Cfg.Node.inputs blk cfg)
    then [blk]
    else [] in
  if Insn.(is call) term then
    Seq.fold ~init (Cfg.Node.outputs blk cfg)
      ~f:(fun rs e ->
          if Cfg.Edge.label e <> `Fall then Cfg.Edge.dst e :: rs
          else rs)
  else init

let find_calls cfg roots =
  let roots = List.fold ~init:Addr.Set.empty ~f:Set.add roots in
  Graphlib.depth_first_search (module Cfg)
    cfg ~init:Block.Set.empty
    ~enter_node:(fun _ blk all ->
        roots_of_blk roots cfg blk |>
        List.fold ~init:all ~f:Set.add)

let reconstruct name roots cfg =
  let roots = find_calls cfg roots in
  let filtered = Set.fold roots ~init:cfg
      ~f:(fun g root ->
          let inputs = Cfg.Node.inputs root cfg in
          Seq.fold inputs ~init:g ~f:(fun g e -> Cfg.Edge.remove e g)) in
  Set.fold roots ~init:Symtab.empty
    ~f:(fun syms entry ->
        let name = name (Block.addr entry) in
        let cfg : cfg =
          with_return (fun {return} ->
              Graphlib.depth_first_search (module Cfg)
                filtered ~start:entry ~init:Cfg.empty
                ~enter_edge:(fun _ -> Cfg.Edge.insert)
                ~start_tree:(fun n t ->
                    if Block.equal n entry
                    then Cfg.Node.insert n t
                    else return t)) in
        Symtab.add_symbol syms (name,entry,cfg))

let of_blocks syms =
  let reconstruct (cfg : cfg) =
    let blocks = Addr.Table.create () in
    let symtab = String.Table.create () in
    Seq.iter (Cfg.nodes cfg) ~f:(fun blk ->
        Hashtbl.set blocks ~key:(Block.addr blk) ~data:blk);
    Seq.iter syms ~f:(fun (name,b,_) -> match Hashtbl.find blocks b with
        | None -> ()
        | Some blk -> Hashtbl.add_multi symtab ~key:name ~data:blk);
    Hashtbl.fold symtab ~init:Symtab.empty ~f:(fun ~key ~data symtab ->
        List.sort data ~cmp:Block.ascending |> function
        | [] -> symtab
        | entry :: _ as blocks ->
          let g = List.fold blocks ~init:Cfg.empty ~f:(fun g x ->
              List.fold blocks ~init:g ~f:(fun g y ->
                  match Cfg.Node.edge x y cfg with
                  | None -> g
                  | Some e -> Cfg.Edge.insert e g)) in
          if Cfg.Node.mem entry g
          then Symtab.add_symbol symtab (key,entry,g)
          else symtab) in
  create reconstruct


let default name roots = create (reconstruct name roots)

module Factory = Source.Factory.Make(struct type nonrec t = t end)
