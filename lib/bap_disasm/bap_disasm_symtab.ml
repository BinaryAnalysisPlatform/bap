open Bap_core_theory

open Core_kernel
open Regular.Std
open Bap_types.Std
open Image_internal_std
open Or_error

open KB.Syntax
open Format

module Block = Bap_disasm_block
module Cfg = Bap_disasm_rec.Cfg
module Insn = Bap_disasm_insn
module Disasm = Bap_disasm_driver
module Callgraph = Bap_disasm_calls
module Symbolizer = Bap_disasm_symbolizer


type block = Block.t [@@deriving compare, sexp_of]
type edge  = Block.edge  [@@deriving compare, sexp_of]
type cfg = Cfg.t [@@deriving compare]


type fn = string * block * cfg [@@deriving compare]

let sexp_of_fn (name,block,_cfg) =
  Sexp.List [sexp_of_string name; sexp_of_addr (Block.addr block)]

module Fn = Opaque.Make(struct
    type t = fn [@@deriving compare]
    let hash x = String.hash (fst3 x)
  end)


type t = {
  addrs : fn Addr.Map.t;
  names : fn String.Map.t;
  memory : fn Memmap.t;
  ecalls : string Addr.Map.t;
  icalls : string Addr.Map.t;
  extern : Insn.t Map.M(Theory.Label).t;
} [@@deriving sexp_of]


let compare t1 t2 =
  Addr.Map.compare Fn.compare t1.addrs t2.addrs

type symtab = t [@@deriving compare, sexp_of]

let span ((_name,_entry,cfg) as fn) =
  Cfg.nodes cfg |> Seq.fold ~init:Memmap.empty ~f:(fun map blk ->
      Memmap.add map (Block.memory blk) fn)

let empty = {
  addrs = Addr.Map.empty;
  names = String.Map.empty;
  memory = Memmap.empty;
  ecalls = Map.empty (module Addr);
  icalls = Map.empty (module Addr);
  extern = Map.empty (module Theory.Label);
}

let merge m1 m2 =
  Memmap.to_sequence m2 |> Seq.fold ~init:m1 ~f:(fun m1 (mem,x) ->
      Memmap.add m1 mem x)

let filter_mem mem name entry =
  Memmap.filter mem ~f:(fun (n,e,_) ->
      not(String.(name = n) || Block.(entry = e)))

let filter_calls name cfg calls =
  let init = Map.filter calls ~f:(fun name' -> String.(name <> name')) in
  Cfg.nodes cfg |>
  Seq.fold ~init ~f:(fun calls node ->
      Map.remove calls (Block.addr node))

let remove t (name,entry,cfg) : t =
  if Map.mem t.addrs (Block.addr entry) then {
    t with
    names = Map.remove t.names name;
    addrs = Map.remove t.addrs (Block.addr entry);
    memory = filter_mem t.memory name entry;
    ecalls = filter_calls name cfg t.ecalls;
    icalls = filter_calls name cfg t.icalls;
  } else t

let add_symbol t (name,entry,cfg) : t =
  let data = name,entry,cfg in
  let t = remove t data in
  { t with
    addrs = Map.set t.addrs ~key:(Block.addr entry) ~data;
    names = Map.set t.names ~key:name ~data;
    memory = merge t.memory (span data);
  }

let find_by_start tab = Map.find tab.addrs
let find_by_name tab = Map.find tab.names

let fns_of_seq seq =
  Seq.map seq ~f:snd |> Seq.to_list |> Fn.Set.of_list |>
  Set.to_list

let owners t addr = Memmap.lookup t.memory addr |> fns_of_seq
let dominators t mem = Memmap.dominators t.memory mem |> fns_of_seq
let intersecting t mem = Memmap.intersections t.memory mem |> fns_of_seq
let to_sequence t =
  Map.to_sequence t.addrs |> fns_of_seq |> Seq.of_list
let externals t = Map.to_sequence t.extern
let name_of_fn = fst
let entry_of_fn = snd
let span fn = span fn |> Memmap.map ~f:(fun _ -> ())

let insert_call ?(implicit=false) symtab block data =
  let key = Block.addr block in
  if implicit then {
    symtab with
    icalls = Map.set symtab.icalls ~key ~data
  } else {
    symtab with
    ecalls = Map.set symtab.ecalls ~key ~data
  }

let explicit_callee {ecalls} = Map.find ecalls
let implicit_callee {icalls} = Map.find icalls
let callee tab src = match explicit_callee tab src with
  | Some dst -> Some dst
  | None -> implicit_callee tab src

let update_graph calls graphs node f =
  let addr = Block.addr node in
  let entry = Callgraph.entry calls addr in
  let start = Option.some_if (Addr.equal addr entry) node in
  Map.update graphs entry ~f:(function
      | None -> start,f Cfg.empty
      | Some (entry,cfg) -> Option.first_some entry start, f cfg)

let add_node calls graphs node =
  update_graph calls graphs node @@
  Cfg.Node.insert node

let add_edge calls graphs edge =
  update_graph calls graphs (Cfg.Edge.src edge) @@
  Cfg.Edge.insert edge

let collect_graphs disasm calls =
  Disasm.explore disasm
    ~init:(empty,Map.empty (module Addr))
    ~entries:(Set.to_sequence@@Disasm.subroutines disasm)
    ~block:(fun mem insns ->
        Disasm.execution_order insns >>= fun insns ->
        KB.List.filter_map insns ~f:(fun label ->
            KB.collect Theory.Semantics.slot label >>= fun s ->
            KB.collect Memory.slot label >>| function
            | None -> None
            | Some mem -> Some (mem, s)) >>| fun insns ->
        Block.create mem insns)
    ~node:(fun n (tab,graphs) ->
        KB.return (tab, add_node calls graphs n))
    ~edge:(fun src dst (tab,graphs) ->
        let msrc = Block.memory src
        and from = Block.addr src
        and dest = Block.addr dst in
        let next = Addr.succ (Memory.max_addr msrc) in
        if Callgraph.siblings calls (Block.addr src) dest
        then
          let kind = if Addr.equal next dest then `Fall else `Jump in
          let edge = Cfg.Edge.create src dst kind in
          KB.return (tab,add_edge calls graphs edge)
        else
          Symbolizer.get_name (Block.addr dst) >>| fun name ->
          if Addr.equal next dest
          then {tab with icalls = Map.set tab.icalls from name},graphs
          else {tab with ecalls = Map.set tab.ecalls from name},graphs)

let collect_externals disasm =
  Disasm.externals disasm |>
  Set.to_sequence |>
  KB.Seq.fold ~init:(Map.empty (module Theory.Label)) ~f:(fun extern label ->
      let+ insn = label-->Theory.Semantics.slot in
      Map.set extern label insn)

let create disasm calls =
  let* (init,graphs) = collect_graphs disasm calls in
  let* extern = collect_externals disasm in
  let init = {init with extern} in
  Map.to_sequence graphs |>
  KB.Seq.fold ~init ~f:(fun tab (addr,(entry,cfg)) ->
      let+ name = Symbolizer.get_name addr in
      let entry = Option.value_exn entry in
      add_symbol tab (name,entry,cfg))

let result = Toplevel.var "symtab"

module Toplevel = struct
  let create disasm calls =
    Toplevel.put result (create disasm calls);
    Toplevel.get result
end
