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

let terminator_addr blk =
  List.rev (Block.insns blk) |>
  List.hd_exn |>
  fst |>
  Memory.min_addr

let is_fall e = Cfg.Edge.label e = `Fall

let callee_of_edge e =
  if is_fall e then None
  else Some (Cfg.Edge.dst e)

let has_fall_only blk cfg =
  Cfg.Node.degree ~dir:`Out blk cfg = 1 &&
  is_fall (Seq.hd_exn (Cfg.Node.outputs blk cfg))

let add_if_root entries roots blk =
  if Set.mem roots (Block.addr blk) then
    Set.add entries blk
  else entries

let fold_callees blk cfg ~init ~f =
  Cfg.Node.outputs blk cfg |>
  Seq.filter_map ~f:callee_of_edge |>
  Seq.fold ~init ~f

let add_entries entries cfg blk =
  fold_callees blk cfg ~init:entries ~f:Set.add

let add_callees syms name cfg blk =
  let call_addr = terminator_addr blk in
  if has_fall_only blk cfg then
    Symtab.add_callee syms call_addr (name call_addr)
  else
    fold_callees blk cfg ~init:syms ~f:(fun syms c ->
        Symtab.add_callee syms call_addr (name (Block.addr c)))

let collect name cfg roots =
  Seq.fold (Cfg.nodes cfg) ~init:(Block.Set.empty, Symtab.empty)
    ~f:(fun (entries,syms) blk ->
        let entries =
          if Set.mem roots (Block.addr blk) then Set.add entries blk
          else entries in
        if Insn.(is call) (Block.terminator blk) then
          add_entries entries cfg blk,
          add_callees syms name cfg blk
        else entries,syms)

let reconstruct name roots prog =
  let roots = Addr.Set.of_list roots in
  let entries,syms = collect name prog roots in
  let is_call e = Set.mem entries (Cfg.Edge.dst e) in
  let rec add cfg node =
    let cfg = Cfg.Node.insert node cfg in
    Seq.fold (Cfg.Node.outputs node prog) ~init:cfg
      ~f:(fun cfg edge ->
          if is_call edge then cfg
          else
            let cfg' = Cfg.Edge.insert edge cfg in
            if Cfg.Node.mem (Cfg.Edge.dst edge) cfg then cfg'
            else add cfg' (Cfg.Edge.dst edge)) in
  Set.fold entries ~init:syms ~f:(fun syms entry ->
      let name = name (Block.addr entry) in
      let cfg = add Cfg.empty entry in
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
