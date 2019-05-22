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



let (<--) = fun g f -> match g with
  | None -> None
  | Some (e,g) -> Some (e, f g)

let build_cfg disasm calls entry =
  Disasm.explore disasm ~entry ~init:None
    ~follow:(fun dst ->
        KB.return Addr.(Callgraph.entry calls dst = entry))
    ~block:(fun mem insns ->
        Disasm.execution_order insns >>= fun insns ->
        KB.List.filter_map insns ~f:(fun label ->
            KB.collect Theory.Program.Semantics.slot label >>= fun s ->
            KB.collect Memory.slot label >>| function
            | None -> None
            | Some mem -> Some (mem, s)) >>| fun insns ->
        Block.create mem insns)
    ~node:(fun n g ->
        KB.return @@
        if Addr.equal (Block.addr n) entry
        then Some (n,Cfg.Node.insert n Cfg.empty)
        else g <-- Cfg.Node.insert n)
    ~edge:(fun src dst g ->
        let msrc = Block.memory src
        and mdst = Block.memory dst in
        let next = Addr.succ (Memory.max_addr msrc) in
        let kind = if Addr.equal next (Memory.min_addr mdst)
          then `Fall else `Jump in
        let edge = Cfg.Edge.create src dst kind in
        KB.return (g <-- Cfg.Edge.insert edge))

let get_name addr =
  let data = Some (Word.to_bitvec addr) in
  KB.Object.scoped Theory.Program.cls @@ fun label ->
  KB.provide Theory.Label.addr label data >>= fun () ->
  KB.collect Theory.Label.name label >>| function
  | None -> Symbolizer.resolve Symbolizer.empty addr
  | Some name -> name

let build_symbol disasm calls start =
  build_cfg disasm calls start >>= function
  | None -> assert false
  | Some (entry,graph) ->
    get_name start >>| fun name ->
    name,entry,graph

let build disasm =
  Callgraph.update Callgraph.empty disasm >>= fun calls ->
  Callgraph.entries calls |>
  Set.to_sequence |>
  KB.Seq.fold ~init:empty ~f:(fun symtab entry ->
      build_symbol disasm calls entry >>| fun fn ->
      add_symbol symtab fn)

type builer = Builder
let package = "bap.std-internal"
let builder = KB.Class.declare ~package "symtab-builder" Builder
let symtab = KB.Domain.flat ~empty ~equal:(fun x y ->
    compare x y = 0) "symtab"
let result = KB.Class.property ~package builder "result" symtab

let create disasm =
  let query =
    KB.Object.create builder >>= fun builder ->
    build disasm >>= fun symtab ->
    KB.provide result builder symtab >>| fun () ->
    builder in
  KB.Value.get result @@
  Bap_state.run_or_fail builder query
