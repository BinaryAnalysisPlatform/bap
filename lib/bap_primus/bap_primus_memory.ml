open Core_kernel
open Format

open Bap_knowledge
open Bap_core_theory
open Bap_primus_types

module Seq = Sequence

module Observation = Bap_primus_observation
module Generator = Bap_primus_generator
module Machine = Bap_primus_machine
module Value = Bap_primus_value

open Bap_primus_sexp
open Machine.Syntax

type exn += Pagefault of addr
type exn += Uninitialized

let () = Exn.add_printer (function
    | Pagefault here ->
      Some (asprintf "Page fault at %a"
              Word.pp_hex here)
    | Uninitialized ->
      Some (asprintf "Memory system is not initialized")
    | _ -> None)

type region =
  | Dynamic of {base : addr; len : int; value : Generator.t }
  | Static  of {base : addr; data : Bigstring.t; reversed : bool}

type perms = {readonly : bool; executable : bool}
type layer = {mem : region; perms : perms}

type memory = {
  name : string;
  addr : int;
  size : int;
} [@@deriving bin_io, sexp]

let compare_memory {name=x} {name=y} = String.compare x y

module Descriptor = struct
  type t = memory [@@deriving bin_io, compare, sexp]
  let create ~addr_size ~data_size name = {
    addr = addr_size;
    size = data_size;
    name
  }

  let unknown ~addr_size ~data_size =
    create addr_size data_size "unknown"

  let name d = d.name


  include Comparable.Make(struct
      type t = memory [@@deriving bin_io, compare, sexp]
    end)
end

type state = {
  values : value Word.Map.t;
  layers : layer list;
}

type t = {
  curr : Descriptor.t option;
  mems : state Descriptor.Map.t
}

let zero = Word.of_int ~width:8 0

let sexp_of_word w = Sexp.Atom (asprintf "%a" Word.pp_hex w)

let sexp_of_region = function
  | Dynamic {base; len; value} -> Sexp.List [
      Sexp.Atom "dynamic";
      sexp_of_word base;
      sexp_of_int len;
      Generator.sexp_of_t value]
  | Static {base; data} -> Sexp.List [
      Sexp.Atom "static";
      sexp_of_word base;
      sexp_of_int (Bigstring.length data);
    ]

let sexp_of_layer {mem; perms={readonly; executable}} =
  let flags = [
    "R";
    if readonly then "" else "W";
    if executable then "X" else "";
  ] |> String.concat ~sep:"" in
  Sexp.List [sexp_of_region mem; Atom flags]

let inspect_state {values; layers} =
  let values =
    Map.to_sequence values |> Seq.map ~f:(fun (key,value) ->
        Sexp.List [sexp_of_word key; sexp_of_value value]) |>
    Seq.to_list_rev in
  let layers = List.map layers ~f:(sexp_of_layer) in
  Sexp.List [
    List [Atom "values"; List values];
    List [Atom "layers"; List layers];
  ]

let inspect_curr = function
  | None -> Sexp.Atom "<uninitialized>"
  | Some d -> Sexp.Atom (Descriptor.name d)


let inspect_memory {curr; mems} = Sexp.List [
    inspect_curr curr;
    Descriptor.Map.sexp_of_t inspect_state mems;
  ]

let state = Bap_primus_machine.State.declare
    ~uuid:"4b94186d-3ae9-48e0-8a93-8c83c747bdbb"
    ~inspect:inspect_memory
    ~name:"memory" @@ Knowledge.return {
    mems = Descriptor.Map.empty;
    curr = None;
  }

let base (Static {base} | Dynamic {base}) = base
let length = function
  | Static {data} -> Bigstring.length data
  | Dynamic {len} -> len

let contains addr {mem} =
  let base = base mem in
  let high = Word.(base ++ length mem) in
  Word.(addr >= base) && Word.(addr < high)

let find_layer addr = List.find ~f:(contains addr)

let is_mapped addr {layers} = Option.is_some (find_layer addr layers)

let empty_state = {
  values = Word.Map.empty;
  layers = [];
}
let (!!) = Machine.Observation.make

let descriptor = function
  | None -> Machine.raise Uninitialized
  | Some curr -> Machine.return curr


let memory =
  Machine.Local.get state >>= fun s ->
  descriptor s.curr

let switch curr =
  Machine.Local.update state ~f:(fun s -> {s with curr=Some curr})


let get_curr =
  Machine.Local.get state >>= fun {curr; mems} ->
  descriptor curr >>| fun curr ->
  match Map.find mems curr with
  | None -> empty_state
  | Some s -> s

let put_curr mem =
  Machine.Local.get state >>= fun s ->
  descriptor s.curr >>= fun curr ->
  Machine.Local.put state {
    s with
    mems = Map.add s.mems ~key:curr ~data:mem
  }

let update state f =
  Machine.Local.get state >>= fun s ->
  descriptor s.curr >>= fun curr ->
  match Map.find s.mems curr with
  | None -> Machine.return ()
  | Some m -> Machine.Local.put state {
      s with
      mems = Map.add s.mems ~key:curr ~data:(f m)
    }

let pagefault addr = Machine.raise (Pagefault addr)

let read_small ~base ~data addr size =
  assert (size < 8 && size > 0);
  let addr_size = Word.bitwidth addr in
  (* to address {n 2^m} bits we need {m+log(m)} addr space,
     since {n < 8} (it is the word size in bits), we just add 3.*)
  let width = addr_size + 3 in
  let addr = Word.extract_exn ~hi:(width-1) addr in
  let off = Word.(addr - base) in
  let off_in_bits = Word.(off * Word.of_int ~width size) in
  let full_bytes = Word.(off_in_bits / Word.of_int ~width 8) in
  let bit_off =
    Word.to_int_exn @@
    Word.(off_in_bits - full_bytes * Word.of_int ~width 8) in
  let leftover = Bigstring.length data - Word.to_int_exn full_bytes in
  let len = min leftover 2 in
  let full_bytes = Word.extract_exn ~hi:(addr_size-1) full_bytes in
  let pos = Word.to_int_exn full_bytes in
  let data = Bigstring.to_string (Bigstring.sub_shared data ~pos ~len) in
  let vec = Word.of_binary ~width:(len * 8) BigEndian data in
  let hi = len * 8 - bit_off - 1 in
  let lo = hi - size + 1 in
  Word.extract_exn ~hi ~lo vec


let word_of_char x =
  Word.of_int ~width:8 (Char.to_int x)

let read_word base data little addr size =
  let off = Word.(to_int_exn (addr - base)) in
  let start,next = if little
    then off + size/8 - 1, pred
    else off, succ in
  let rec read pos left =
    let data = word_of_char (Bigstring.get data pos) in
    if left <= 8
    then Word.extract_exn ~lo:(8-left) data
    else Word.concat data @@ read (next pos) (left - 8) in
  if size >= 8 then read start size
  else read_small ~base ~data addr size

let read addr {values;layers} = match find_layer addr layers with
  | None -> pagefault addr
  | Some layer -> match Map.find values addr with
    | Some v -> Machine.return v
    | None ->
      memory >>= fun {size} ->
      match layer.mem with
      | Dynamic {value} ->
        Generator.next value >>= Value.of_word
      | Static {base; data; reversed} ->
        Value.of_word (read_word base data reversed addr size)

let write addr value {values;layers} =
  match find_layer addr layers with
  | None -> pagefault addr
  | Some {perms={readonly=true}} -> pagefault addr
  | Some _ -> Machine.return {
      layers;
      values = Map.add values ~key:addr ~data:value;
    }

let add_layer layer t = {t with layers = layer :: t.layers}
let (++) = add_layer

let initialize values base len f =
  Machine.Seq.fold (Seq.range 0 len) ~init:values ~f:(fun values i ->
      let addr = Word.(base ++ i) in
      f addr >>= fun data ->
      Value.of_word data >>| fun data ->
      Map.add values ~key:addr ~data)



let allocate
    ?(executable=false)
    ?(readonly=false)
    ?init
    ?generator
    base len =
  memory >>= fun {size} ->
  let value = match generator with
    | None -> Generator.random size
    | Some other -> other in
  get_curr >>| add_layer {
    perms={readonly; executable};
    mem = Dynamic {base;len; value}
  } >>= fun s ->
  match init with
  | None -> put_curr s
  | Some f ->
    initialize s.values base len f >>= fun values ->
    put_curr {s with values}

let map
    ?(executable=false)
    ?(readonly=false)
    ?(reversed=false)
    base data =
  update state @@ add_layer {
    mem=Static {data; base; reversed};
    perms={readonly; executable}
  }
let add_text mem = map mem ~readonly:true  ~executable:true
let add_data mem = map mem ~readonly:false ~executable:false

let get addr = get_curr >>= read addr

let set addr value =
  get_curr >>=
  write addr value >>=
  put_curr

let load addr = get addr >>| Value.to_word
let store addr value = Value.of_word value >>= set addr

let is_mapped addr =
  get_curr >>| is_mapped addr

let is_writable addr =
  get_curr >>| fun {layers} ->
  find_layer addr layers |>
  function Some {perms={readonly}} -> not readonly
         | None -> false
