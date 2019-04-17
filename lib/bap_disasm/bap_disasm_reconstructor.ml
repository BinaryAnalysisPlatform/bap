open Core_kernel
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

   A block is considered to be a function start if one of the
   following is true:
   - the block address belongs to the provided set of roots;
   - the block has no incoming edges

   In general, all blocks should be reachable from the set of roots,
   thus a block without incoming edges should belong to the set of
   roots, so it might be tempting to say that the second clause is
   redundant.

   However, our implementation of the recursive descent disassembler
   can generate blocks that are not reachable from the initial set of
   roots. This can happen only if the set of roots is empty, which is
   treated as a special case, that instructs the disassembler to treat
   the first byte of the provided input as a root.

   Beyond being a questionable design decision, this behavior is
   actually just an example of a more general problem. The
   reconstructor and disassemblers could be called with different sets
   of roots. So, in general, we can't rely on the set of roots passed
   to the reconstructor for functions start classification.*)
let is_start roots cfg blk =
  Set.mem roots (Block.addr blk) ||
  Cfg.Node.degree ~dir:`In blk cfg = 0

let is_fall e = Cfg.Edge.label e = `Fall
let is_call b = Insn.(is call) (Block.terminator b)

let entries_of_block cfg roots blk =
  let entries =
    if is_start roots cfg blk then Block.Set.singleton blk
    else Block.Set.empty in
  if is_call blk then
    Seq.fold ~init:entries (Cfg.Node.outputs blk cfg)
      ~f:(fun entries e ->
          if is_fall e then entries
          else Set.add entries (Cfg.Edge.dst e))
  else entries

let terminator_addr blk =
  List.rev (Block.insns blk) |>
  List.hd_exn |>
  fst |>
  Memory.min_addr

let is_unresolved blk cfg =
  let deg = Cfg.Node.degree ~dir:`Out blk cfg in
  deg = 0 ||
  (deg = 1 && is_fall (Seq.hd_exn (Cfg.Node.outputs blk cfg)))

let add_call symtab blk name label =
  Symtab.add_call symtab blk name label

let add_unresolved syms name cfg blk =
  if is_unresolved blk cfg then
    let call_addr = terminator_addr blk in
    add_call syms blk (name call_addr) `Fall
  else syms

let collect name cfg roots =
  Seq.fold (Cfg.nodes cfg) ~init:(Block.Set.empty, Symtab.empty)
    ~f:(fun (entries, syms) blk ->
        let entries' = entries_of_block cfg roots blk in
        Set.union entries entries', add_unresolved syms name cfg blk)

let reachable cfg from =
  let rec loop nodes node =
    Seq.fold (Cfg.Node.outputs node cfg)
      ~init:(Set.add nodes node)
      ~f:(fun nodes edge ->
          if Set.mem nodes ( Cfg.Edge.dst edge) then nodes
          else loop nodes (Cfg.Edge.dst edge)) in
  loop Block.Set.empty from

let sub roots prog start =
  let is_call e = Set.mem roots (Cfg.Edge.dst e) in
  let update_inputs node init =
    Seq.fold ~init (Cfg.Node.inputs node prog) ~f:Set.add in
  let rec loop cfg inputs node =
    Seq.fold (Cfg.Node.outputs node prog)
      ~init:(cfg, update_inputs node inputs)
      ~f:(fun (cfg, inputs) edge ->
          if is_call edge then cfg,inputs
          else
            let cfg' = Cfg.Edge.insert edge cfg in
            if Cfg.Node.mem (Cfg.Edge.dst edge) cfg then cfg',inputs
            else loop cfg' inputs (Cfg.Edge.dst edge)) in
  let cfg = Cfg.Node.insert start Cfg.empty in
  loop cfg Cfg.Edge.Set.empty start

let edges_of_seq s = Seq.fold s ~init:Cfg.Edge.Set.empty ~f:Set.add

let reconstruct name initial_roots prog =
  let (--) = Set.diff in
  let update_symtab syms cfg entry inputs =
    let name = name (Block.addr entry) in
    let syms = Symtab.add_symbol syms (name,entry,cfg) in
    Set.fold inputs ~init:syms ~f:(fun syms e ->
        add_call syms (Cfg.Edge.src e) name (Cfg.Edge.label e)) in
  let remove_node cfg n = Cfg.Node.remove n cfg in
  let remove_reachable cfg from =
    let reachable = reachable cfg from in
    let cfg = Set.fold reachable ~init:cfg ~f:remove_node in
    remove_node cfg from in
  let collect_destinations edges =
    Set.fold edges ~init:Block.Set.empty ~f:(fun bs e ->
        Set.add bs (Cfg.Edge.dst e)) in
  let rec loop known_roots syms = function
    | [] -> syms,known_roots
    | root :: roots ->
      let self_inputs = edges_of_seq (Cfg.Node.inputs root prog) in
      let cfg, inputs = sub known_roots prog root in
      let edges = edges_of_seq (Cfg.edges cfg) in
      let calls = collect_destinations (inputs -- edges -- self_inputs) in
      let cfg   = Set.fold calls ~init:cfg ~f:remove_reachable in
      let known = Set.union known_roots calls in
      let syms  = update_symtab syms cfg root self_inputs  in
      let syms,known = loop known syms (Set.to_list calls) in
      loop known syms roots in
  let initial_roots = Addr.Set.of_list initial_roots in
  let roots,syms = collect name prog initial_roots in
  fst @@ loop roots syms (Set.to_list roots)

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
        List.sort data ~compare:Block.ascending |> function
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
