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

let add_callnames syms name cfg blk =
  if is_call blk then
    let call_addr = terminator_addr blk in
    if is_unresolved blk cfg then
      Symtab.add_call_name syms blk (name call_addr)
    else
      Seq.fold ~init:syms (Cfg.Node.outputs blk cfg)
        ~f:(fun syms e ->
            if is_fall e then syms
            else
              Cfg.Edge.dst e |> Block.addr |> name |>
              Symtab.add_call_name syms blk)
  else syms

let add_call_addrs syms cfg entries =
  Set.fold entries ~init:syms ~f:(fun syms b ->
      Seq.fold (Cfg.Node.inputs b cfg) ~init:syms
        ~f:(fun syms e ->
            match Cfg.Edge.label e with
            | `Fall ->
               Symtab.add_call_addr syms (Cfg.Edge.src e) (Block.addr b)
            | _ -> syms))

let collect name cfg roots =
  Seq.fold (Cfg.nodes cfg) ~init:(Block.Set.empty, Symtab.empty)
    ~f:(fun (entries, syms) blk ->
        let entries' = entries_of_block cfg roots blk in
        let syms = add_call_addrs syms cfg entries' in
        Set.union entries entries', add_callnames syms name cfg blk)

let remove_by_start symtab start =
  match Symtab.find_by_start symtab (Block.addr start) with
  | None -> symtab
  | Some fn -> Symtab.remove symtab fn

type info = {
    invalid : Block.Set.t;
    visited : block Addr.Map.t;
    discovered : Block.Set.t;
  }

let empty_info =
  { visited = Addr.Map.empty;
    invalid = Block.Set.empty;
    discovered = Block.Set.empty; }

let visit n e i = {i with visited = Map.set i.visited (Block.addr n) e}
let invalidate n i = {i with invalid = Set.add i.invalid n}
let discover n i = {i with discovered = Set.add i.discovered n}

let sub_cfg info roots prog start =
  let is_call e = Set.mem roots (Cfg.Edge.dst e) in
  let find_start n i = Map.find i.visited (Block.addr n) in
  let insert_edge cfg info edge =
    match find_start (Cfg.Edge.dst edge) info with
    | Some start' when Block.(start <> start') ->
       None, invalidate start' info |> discover (Cfg.Edge.dst edge)
    | _ -> Some (Cfg.Edge.insert edge cfg), info in
  let rec loop cfg info node =
    let info = visit node start info in
    Seq.fold (Cfg.Node.outputs node prog)
      ~init:(cfg, info)
      ~f:(fun (cfg, info) edge ->
          if is_call edge then cfg,info
          else
            match insert_edge cfg info edge with
            | None, info -> cfg,info
            | Some cfg', info ->
              if Cfg.Node.mem (Cfg.Edge.dst edge) cfg then cfg',info
              else loop cfg' info (Cfg.Edge.dst edge)) in
  loop (Cfg.Node.insert start Cfg.empty) info start

let add_sym name syms cfg entry =
  Symtab.add_symbol syms (name (Block.addr entry),entry,cfg)

let reconstruct name roots prog =
  let rec loop entries syms info =
    let syms, info =
      Set.fold info.discovered ~init:(syms,info)
        ~f:(fun (syms,info) entry ->
          let cfg,info = sub_cfg info entries prog entry in
          add_sym name syms cfg entry, info) in
    if Set.is_empty info.invalid then syms
    else
      let syms = Set.fold info.invalid ~init:syms ~f:remove_by_start in
      let roots = Set.union info.invalid info.discovered in
      let info = {empty_info with discovered=roots} in
      loop (Set.union entries roots) syms info in
  let roots = Addr.Set.of_list roots in
  let roots,syms = collect name prog roots in
  loop roots syms { empty_info with discovered=roots}

let reachable cfg start =
  let rec loop nodes node =
    Seq.fold (Cfg.Node.outputs node cfg)
      ~init:(Set.add nodes node)
      ~f:(fun nodes edge ->
        if Set.mem nodes (Cfg.Edge.dst edge) then nodes
        else loop nodes (Cfg.Edge.dst edge)) in
  loop Block.Set.empty start

let slice roots prog start =
  let is_call e = Set.mem roots (Cfg.Edge.dst e) in
  let rec loop cfg inputs node =
    let inputs = Seq.fold ~init:inputs
                  (Cfg.Node.inputs node prog) ~f:Set.add in
    Seq.fold (Cfg.Node.outputs node prog)
      ~init:(cfg, inputs)
      ~f:(fun (cfg, edges) edge ->
          if is_call edge then cfg,edges
          else
            let cfg' = Cfg.Edge.insert edge cfg in
            if Cfg.Node.mem (Cfg.Edge.dst edge) cfg then cfg',edges
            else loop cfg' edges (Cfg.Edge.dst edge)) in
  let cfg = Cfg.empty |> Cfg.Node.insert start in
  loop cfg Cfg.Edge.Set.empty start

let reconstruct name initial_roots prog =
  let rec loop known_roots syms = function
    | [] -> syms,known_roots
    | root :: roots ->
       let cfg,edges' = slice known_roots prog root in
       let edges = Cfg.Edge.Set.of_list (Seq.to_list (Cfg.edges cfg)) in
       let side_calls = Set.diff edges' edges in
       let discovered =
         Set.fold side_calls ~init:Block.Set.empty ~f:(fun bs e -> Set.add bs (Cfg.Edge.dst e)) in
       let discovered = Set.remove discovered root in
       let cfg =
         Set.fold discovered ~init:cfg  ~f:(fun cfg r ->
             reachable cfg r |>
             Set.fold ~init:cfg ~f:(fun cfg n -> Cfg.Node.remove n cfg)) in
       let known_roots = Set.union known_roots discovered in
       let syms = add_sym name syms cfg root in
       let syms,known' = loop known_roots syms (Set.to_list discovered) in
       loop (Set.union known' known_roots) syms roots in
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
