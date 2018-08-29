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

(* [is_start roots cfg blk] returns true if [blk] is a function start.
   Block can be a function start in the next cases:
   - block address is in a set of [roots]
   - block doesn't have any input edges in [cfg].
   The later is extremly important in case of pure code input. *)
let is_start roots cfg blk =
  Set.mem roots (Block.addr blk) ||
  Cfg.Node.degree ~dir:`In blk cfg = 0

let entries_of_block cfg roots entries blk =
  let entries =
    if is_start roots cfg blk then Set.add entries blk
    else entries in
  let term = Block.terminator blk in
  if Insn.(is call) term then
    Seq.fold ~init:entries (Cfg.Node.outputs blk cfg)
      ~f:(fun entries e ->
          if Cfg.Edge.label e <> `Fall then
            Set.add entries (Cfg.Edge.dst e)
          else entries)
  else entries

let collect_entries cfg roots =
  let roots = Addr.Set.of_list roots in
  Seq.fold (Cfg.nodes cfg) ~init:Block.Set.empty
    ~f:(entries_of_block cfg roots)

let reconstruct name roots prog =
  let entries = collect_entries prog roots in
  let is_call e = Set.mem entries (Cfg.Edge.dst e) in
  let rec add cfg node =
    let cfg = Cfg.Node.insert node cfg in
    Seq.fold (Cfg.Node.outputs node prog) ~init:cfg ~f:(fun cfg edge ->
        if is_call edge then cfg
        else
          let cfg' = Cfg.Edge.insert edge cfg in
          if Cfg.Node.mem (Cfg.Edge.dst edge) cfg then cfg'
          else add cfg' (Cfg.Edge.dst edge)) in
  Set.fold entries ~init:Symtab.empty ~f:(fun tab entry ->
      let name = name (Block.addr entry) in
      let fng = add Cfg.empty entry in
      Symtab.add_symbol tab (name,entry,fng))

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
