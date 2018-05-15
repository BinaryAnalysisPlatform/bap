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

let find_calls name roots cfg =
  let starts = Addr.Table.create () in
  List.iter roots ~f:(fun addr ->
      Hashtbl.set starts ~key:addr ~data:(name addr));
  Cfg.nodes cfg |> Seq.iter ~f:(fun blk ->
      let () =
        if Seq.is_empty (Cfg.Node.inputs blk cfg) then
          let addr = Block.addr blk in
          Hashtbl.set starts ~key:addr ~data:(name addr) in
      let term = Block.terminator blk in
      if Insn.(is call) term then
        Seq.iter (Cfg.Node.outputs blk cfg)
          ~f:(fun e ->
              if Cfg.Edge.label e <> `Fall then
                let w = Block.addr (Cfg.Edge.dst e) in
                Hashtbl.set starts ~key:w ~data:(name w)));
  starts

let reconstruct name roots cfg =
  let roots = find_calls name roots cfg in
  let init =
    Cfg.nodes cfg |> Seq.fold ~init:Cfg.empty ~f:(fun cfg n ->
        Cfg.Node.insert n cfg) in
  let filtered =
    Cfg.edges cfg |> Seq.fold ~init ~f:(fun cfg e ->
        if Hashtbl.mem roots (Block.addr (Cfg.Edge.dst e)) then cfg
        else Cfg.Edge.insert e cfg) in
  let find_block addr =
    Cfg.nodes cfg |> Seq.find ~f:(fun blk ->
        Addr.equal addr (Block.addr blk)) in
  Hashtbl.fold roots ~init:Symtab.empty
    ~f:(fun ~key:entry ~data:name syms ->
        match find_block entry with
        | None -> syms
        | Some entry ->
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
