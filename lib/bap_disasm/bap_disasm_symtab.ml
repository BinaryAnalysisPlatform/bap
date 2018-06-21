open Core_kernel.Std
open Regular.Std
open Bap_types.Std
open Image_internal_std
open Or_error

open Format

module Block = Bap_disasm_block
module Cfg = Bap_disasm_rec.Cfg
module Insn = Bap_disasm_insn


type block = Block.t [@@deriving compare, sexp_of]
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
}

let merge m1 m2 =
  Memmap.to_sequence m2 |> Seq.fold ~init:m1 ~f:(fun m1 (mem,x) ->
      Memmap.add m1 mem x)

let filter_mem mem name entry =
  Memmap.filter mem ~f:(fun (n,e,_) ->
      not(String.(name = n) || Block.(entry = e)))

let remove t (name,entry,_) : t = {
  names = Map.remove t.names name;
  addrs = Map.remove t.addrs (Block.addr entry);
  memory = filter_mem t.memory name entry;
}

let filter t ((name,entry,_ ) as fn) =
  if Map.mem t.names name || Map.mem t.addrs (Block.addr entry) then
    remove t fn
  else t

let add_symbol t (name,entry,cfg) : t =
  let data = name,entry,cfg in
  let t = filter t data in
  {
    addrs = Map.add t.addrs ~key:(Block.addr entry) ~data;
    names = Map.add t.names ~key:name ~data;
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
