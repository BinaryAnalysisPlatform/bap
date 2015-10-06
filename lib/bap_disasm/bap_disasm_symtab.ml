open Core_kernel.Std
open Bap_types.Std
open Image_internal_std
open Or_error

module Block = Bap_disasm_block
module Insn = Bap_disasm_insn
type block = Block.t with compare,sexp_of


type fn = string * block with compare, sexp_of

type t = {
  memory : fn memmap;
  entries: fn Addr.Map.t;
  names : addr String.Map.t;
}

let recons_symbol starts ((name,entry) as fn) table : fn memmap =
  let overran =
    match Map.next_key starts (Block.addr entry) with
    | None -> (fun _ -> false)
    | Some (next,_) -> (fun blk -> Block.addr blk >= next) in
  let rec loop vis table p =
    let is_call = Insn.is_call (Block.terminator p) in
    Seq.fold ~init:(vis,table) (Block.dests p)
      ~f:(fun (vis,table) -> function
          | `Unresolved _ -> vis,table
          | `Block (blk,`Fall) when is_call && overran blk -> vis,table
          | `Block (_,(`Jump|`Cond)) when is_call -> vis,table
          | `Block (blk,_) ->
            if Set.mem vis (Block.addr blk)
            then vis,table
            else
              let table = Memmap.add table (Block.memory blk) fn in
              let vis = Set.add vis (Block.addr blk) in
              loop vis table blk) in
  let vis = Set.add Addr.Set.empty (Block.addr entry) in
  let table = Memmap.add table (Block.memory entry) fn in
  loop vis table entry |> snd

let dest_of_bil bil =
  (object inherit [word] Bil.finder
    method! enter_jmp dst goto = match dst with
      | Bil.Int dst -> goto.return (Some dst)
      | _ -> goto
  end)#find_in_bil bil

let dest_of_insn insn =
  match Insn.bil insn with
  | _ :: _ as bil -> dest_of_bil bil
  | [] -> None

let name_of_addr addr =
  sprintf "sub_%s" @@ Addr.string_of_value addr

let noname _ = None

let find_entries ?(name=noname) ?(roots=[]) cfg =
  let name addr = match name addr with
    | Some name -> name
    | None -> name_of_addr addr in
  let map = List.fold roots ~init:Addr.Map.empty ~f:(fun map addr ->
      Map.add map ~key:addr ~data:(name addr)) in
  Table.fold cfg ~init:map ~f:(fun blk map ->
      let term = Block.terminator blk in
      if Insn.is_call term then match dest_of_insn term with
        | None -> map
        | Some w -> Map.add map ~key:w ~data:(name w)
      else map)

let empty = {
  entries = Addr.Map.empty;
  memory  = Memmap.empty;
  names = String.Map.empty;
}

let reconstruct ?name ?roots cfg : t =
  let roots = find_entries ?name ?roots cfg in
  Table.fold cfg ~init:empty ~f:(fun blk tab ->
      let addr = Block.addr blk in
      Option.value_map ~default:tab
        (Map.find roots (Block.addr blk)) ~f:(fun name -> {
              names = Map.add tab.names ~key:name ~data:addr;
              memory = recons_symbol roots (name,blk) tab.memory;
              entries = Map.add tab.entries ~key:addr ~data:(name,blk)
            }))


let add_symbol t name entry blocks : t =
  let addr = Block.addr entry in
  let data = (name,entry) in
  let memory = Seq.fold (entry ^:: blocks) ~init:t.memory
      ~f:(fun map blk -> Memmap.add map (Block.memory blk) data) in
  {
    entries = Map.add t.entries ~key:addr ~data;
    names = Map.add t.names ~key:name ~data:addr;
    memory
  }

let remove t ((name,entry) as fn) : t =
  let memory = Memmap.filter t.memory ~f:(fun f ->
      (compare_fn f fn = 0)) in
  {
    memory;
    names = Map.remove t.names name;
    entries = Map.remove t.entries (Block.addr entry);
  }

let find_by_start tab = Map.find tab.entries
let find_by_name tab name =
  Option.(Map.find tab.names name >>= find_by_start tab)

let memory_of_fn tab fn =
  Memmap.filter_map tab.memory
    ~f:(fun fn' ->
        Option.some_if (compare_fn fn fn' = 0) ())

let create_bound tab fn =
  let map = memory_of_fn tab fn in
  stage (Memmap.contains map)

let fns_of_seq seq = Seq.map seq ~f:snd |> Seq.to_list
let fns_of_addr t addr = Memmap.lookup t.memory addr |> fns_of_seq
let fns_of_mem t mem = Memmap.dominators t.memory mem |> fns_of_seq
let to_sequence t = Map.to_sequence t.entries |> Seq.map ~f:snd
let name_of_fn = fst
let entry_of_fn = snd
