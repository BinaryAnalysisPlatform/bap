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
        let syms = add_call_addrs syms cfg entries in
        Set.union entries entries', add_callnames syms name cfg blk)

let edge_to_string e =
  let bs = Block.to_string in
  let label = Sexp.to_string (Block.sexp_of_edge (Cfg.Edge.label e)) in
  let src = bs @@ Cfg.Edge.src e in
  let dst = bs @@ Cfg.Edge.dst e in
  sprintf "[%s - %s(%s)]" src dst label

let remove_by_start symtab start =
  match Symtab.find_by_start symtab (Block.addr start) with
  | None -> symtab
  | Some ((name,_,_) as fn) ->
     printf "remove %s at %s from symtab\n"
       name (Block.to_string start);

     Symtab.remove symtab fn

let add_sym name syms cfg entry =
  Symtab.add_symbol syms (name (Block.addr entry),entry,cfg)


type info = {
    roots : Block.Set.t;
    wrong : Block.Set.t;
    visited : block Addr.Map.t;
  }

let empty_info =
  { visited=Addr.Map.empty;
    roots=Block.Set.empty;
    wrong=Block.Set.empty }

let visit n e i  = {i with visited = Map.set i.visited (Block.addr n) e}
let wrong n i = {i with wrong = Set.add i.wrong n}
let root  n i = {i with roots = Set.add i.roots n}

let sub_cfg info entries prog entry =
  let is_call e = Set.mem entries (Cfg.Edge.dst e) in
  let visit node info = Option.map info ~f:(visit node entry) in
  let wrong node info = Option.map info ~f:(wrong node) in
  let root  node info = Option.map info ~f:(root  node) in
  let visited n = function
    | None -> None
    | Some i -> Map.find i.visited (Block.addr n) in
  let insert_edge cfg info edge =
    match visited (Cfg.Edge.dst edge) info with
    | Some entry' when Block.(entry <> entry') ->
       printf
         "block %s was seen while loop in %s, so it didn't insert in %s\n"
         (Block.to_string (Cfg.Edge.dst edge))
         (Block.to_string entry')
         (Block.to_string entry);
       None, wrong entry' info |> root (Cfg.Edge.dst edge)
    | _ ->
       printf
         "INSERT block %s while loop in %s\n"
         (Block.to_string (Cfg.Edge.dst edge)) (Block.to_string entry);
       Some (Cfg.Edge.insert edge cfg), info in
  let rec loop cfg info node =
      let info = visit node info in
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
  loop (Cfg.Node.insert entry Cfg.empty) info entry

let sub_cfg' a b c = fst @@ sub_cfg None a b c

let reconstruct name roots prog =
  let roots = Addr.Set.of_list roots in
  let entries,syms = collect name prog roots in
  let syms, info =
    Set.fold entries ~init:(syms, Some empty_info)
      ~f:(fun (syms,info) entry ->
        let cfg,info = sub_cfg info entries prog entry in
        add_sym name syms cfg entry, info) in
  match info with
  | None -> syms
  | Some {wrong;roots} ->
     printf "new rooots: \n";
     Set.iter roots ~f:(fun b -> printf "%s\n" (Block.to_string b));
     printf "\n\nwrong: \n";
     Set.iter wrong ~f:(fun b -> printf "%s\n" (Block.to_string b));
     printf "\n\n";
     let syms = Set.fold wrong ~init:syms ~f:remove_by_start in
     let entries = Set.union entries roots in
     Set.fold (Set.union roots wrong) ~init:syms
       ~f:(fun syms start ->
         let cfg = sub_cfg' entries prog start in
         add_sym name syms cfg start)

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
