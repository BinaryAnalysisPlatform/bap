open Core_kernel
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

type callee = addr option * string option
[@@deriving sexp_of]

type t = {
  addrs : fn Addr.Map.t;
  names : fn String.Map.t;
  memory : fn Memmap.t;
  callees : callee Addr.Map.t;
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
  callees = Addr.Map.empty;
}

let merge m1 m2 =
  Memmap.to_sequence m2 |> Seq.fold ~init:m1 ~f:(fun m1 (mem,x) ->
      Memmap.add m1 mem x)

let filter_mem mem name entry =
  Memmap.filter mem ~f:(fun (n,e,_) ->
      not(String.(name = n) || Block.(entry = e)))

let filter_callees name addr =
  Map.filter ~f:(function
      | Some a, Some n -> Addr.(addr <> a) && String.(name <> n)
      | Some a, _ -> Addr.(addr <> a)
      | _, Some n -> String.(name <> n)
      | _ -> true)

let remove t (name,entry,_) : t =
  if Map.mem t.addrs (Block.addr entry) then
    {
      names = Map.remove t.names name;
      addrs = Map.remove t.addrs (Block.addr entry);
      memory = filter_mem t.memory name entry;
      callees = filter_callees name (Block.addr entry) t.callees;
    }
  else t

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

let update_callee t b addr name =
  let set ~default update =
    if Option.is_some update then update else default in
  { t with
    callees =
      Map.update t.callees (Block.addr b)
        ~f:(function
            | None -> addr, name
            | Some (a,n) -> set ~default:a addr, set ~default:n name)}

let add_call_name t b name = update_callee t b None (Some name)
let add_call_addr t b addr = update_callee t b (Some addr) None
let get t addr ~f = Option.map ~f (Map.find t.callees addr)
let find_call_addr t addr = get t addr ~f:fst
let find_call_name t addr = get t addr ~f:snd
