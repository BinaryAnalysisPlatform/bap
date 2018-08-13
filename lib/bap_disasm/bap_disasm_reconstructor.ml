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

let callees_of_block cfg roots blk =
  let addr = Block.addr blk in
  let term = Block.terminator blk in
  let init =
    if Set.mem roots addr then Block.Set.singleton blk
    else Block.Set.empty in
  if Insn.(is call) term then
    Seq.fold ~init (Cfg.Node.outputs blk cfg)
      ~f:(fun cls e ->
          if Cfg.Edge.label e <> `Fall then
            Set.add cls (Cfg.Edge.dst e)
          else cls)
  else init

let update_callees cfg roots callees blk =
  Set.union callees (callees_of_block cfg roots blk)

let find_callees cfg roots =
  let roots = Addr.Set.of_list roots in
  Seq.fold (Cfg.nodes cfg) ~init:Block.Set.empty
    ~f:(update_callees cfg roots)

let reconstruct name roots cfg =
  let callees = find_callees cfg roots in
  let is_call e = Set.mem callees (Cfg.Edge.dst e) in
  let rec traverse fng node =
    let fng = Cfg.Node.insert node fng in
    Seq.fold (Cfg.Node.outputs node cfg) ~init:fng ~f:(fun fng edg ->
        if is_call edg then fng
        else
          let dst = Cfg.Edge.dst edg in
          let visited = Cfg.Node.mem dst fng in
          let fng = Cfg.Edge.insert edg fng in
          if visited then fng
          else traverse fng dst) in
  Set.fold callees ~init:Symtab.empty ~f:(fun tab entry ->
      let name = name (Block.addr entry) in
      let fng = traverse Cfg.empty entry in
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
