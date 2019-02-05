open Base
open Bigarray
open Caml.Format

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

type data = (char, int8_unsigned_elt, c_layout) Array1.t


let () = Exn.add_printer (function
    | Pagefault here ->
      Some (asprintf "Page fault at %a"
              Bitvec.pp here)
    | Uninitialized ->
      Some (asprintf "The memory system is not initialized")
    | _ -> None)

type region =
  | Dynamic of {base : addr; len : int; value : Generator.t }
  | Static  of {base : addr; data : data; reversed : bool}

type perms = {readonly : bool; executable : bool}
type layer = {mem : region; perms : perms}

type memory = {
  name : string;
  addr_size : int;
  data_size : int;
} [@@deriving sexp]

let compare_memory {name=x} {name=y} = String.compare x y

module Descriptor = struct
  type t = memory [@@deriving compare, sexp]
  let create ~addr_size ~data_size name = {
    addr_size;
    data_size;
    name
  }

  let unknown ~addr_size ~data_size =
    create addr_size data_size "unknown"

  let name d = d.name


  include Comparable.Make(struct
      type t = memory [@@deriving compare, sexp]
    end)
end

type state = {
  values : value Map.M(Word).t;
  layers : layer list;
}

type t = {
  curr : Descriptor.t option;
  mems : state Map.M(Descriptor).t
}

let zero = Bitvec.zero

let sexp_of_word = Bitvec_sexp.sexp_of_t

let sexp_of_region = function
  | Dynamic {base; len; value} -> Sexp.List [
      Sexp.Atom "dynamic";
      sexp_of_word base;
      sexp_of_int len;
      Generator.sexp_of_t value]
  | Static {base; data} -> Sexp.List [
      Sexp.Atom "static";
      sexp_of_word base;
      sexp_of_int (Array1.dim data);
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
    [%sexp_of: Map.M(Descriptor).t] inspect_state mems;
  ]

let state = Bap_primus_machine.State.declare
    ~uuid:"4b94186d-3ae9-48e0-8a93-8c83c747bdbb"
    ~inspect:inspect_memory
    ~name:"memory" @@ Knowledge.return {
    mems = Map.empty (module Descriptor);
    curr = None;
  }

let base (Static {base} | Dynamic {base}) = base
let length = function
  | Static {data} -> Array1.dim data
  | Dynamic {len} -> len

let contains addr size {mem} =
  let m = Bitvec.modulus size in
  let base = base mem in
  let high = Bitvec.((base ++ length mem) mod m) in
  Bitvec.(addr >= base) && Bitvec.(addr < high)

let find_layer addr size = List.find ~f:(contains addr size)

let empty_state = {
  values = Map.empty (module Bitvec_order);
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


let with_curr f =
  Machine.Local.get state >>= fun {curr; mems} ->
  descriptor curr >>= fun curr ->
  match Map.find mems curr with
  | None -> f curr empty_state
  | Some s -> f curr s

let get_curr = with_curr (fun _ c -> Machine.return c)

let put_curr mem =
  Machine.Local.get state >>= fun s ->
  descriptor s.curr >>= fun curr ->
  Machine.Local.put state {
    s with
    mems = Map.set s.mems ~key:curr ~data:mem
  }

let update state f =
  Machine.Local.get state >>= fun s ->
  descriptor s.curr >>= fun curr ->
  match Map.find s.mems curr with
  | None -> Machine.return ()
  | Some m -> Machine.Local.put state {
      s with
      mems = Map.set s.mems ~key:curr ~data:(f m)
    }

let pagefault addr = Machine.raise (Pagefault addr)

let bitvec_of_data ~pos ~len (data : data) =
  Bitvec.concat 8 @@
  List.init len ~f:(fun i ->
      let x = Char.to_int data.{pos + i} in
      Bitvec.(int x mod m8))

let read_small ~base ~data ~addr_size addr size =
  assert (size < 8 && size > 0);
  (* to address {n 2^m} bits we need {m+log(m)} addr space,
     since {n < 8} (it is the word size in bits), we just add 3.*)
  let width = addr_size + 3 in
  let m = Bitvec.modulus (width + 3) in
  let addr = Bitvec.extract ~hi:(width-1) ~lo:0 addr in
  let off = Bitvec.((addr - base) mod m) in
  let b8 = Bitvec.(int 8 mod m) in
  let off_in_bits = Bitvec.(off * (int size mod m) mod m) in
  let full_bytes = Bitvec.((off_in_bits / b8) mod m) in
  let bit_off =
    Bitvec.to_int @@
    Bitvec.((off_in_bits - (full_bytes * b8) mod m) mod m) in
  let leftover = Array1.dim data - Bitvec.to_int full_bytes in
  let len = min leftover 2 in
  let full_bytes = Bitvec.extract ~hi:(addr_size-1) ~lo:0 full_bytes in
  let pos = Bitvec.to_int full_bytes in
  let vec = bitvec_of_data ~pos ~len data in
  let hi = len * 8 - bit_off - 1 in
  let lo = hi - size + 1 in
  Bitvec.extract ~hi ~lo vec


let word_of_char x = Bitvec.(int (Char.to_int x) mod m8)

let read_word base data addr_size little addr size =
  let m = Bitvec.modulus addr_size in
  let off = Bitvec.(to_int @@ (addr - base) mod m) in
  let start,next = if little
    then off + size/8 - 1, Int.pred
    else off, Int.succ in
  let rec read pos left =
    let data = word_of_char data.{pos} in
    if left <= 8
    then Bitvec.extract ~lo:(8-left) ~hi:0 data
    else Bitvec.append 8 (left - 8) data @@
      read (next pos) (left - 8) in
  if size >= 8 then read start size
  else read_small ~base ~data ~addr_size addr size

let read addr {addr_size; data_size} {values;layers} =
  match find_layer addr addr_size layers with
  | None -> pagefault addr
  | Some layer -> match Map.find values addr with
    | Some v -> Machine.return v
    | None -> match layer.mem with
      | Dynamic {value} ->
        Generator.next value >>= Value.word
      | Static {base; data; reversed} ->
        Value.word @@
        read_word base data addr_size reversed addr data_size

let write addr value {addr_size} {values;layers} =
  match find_layer addr addr_size layers with
  | None -> pagefault addr
  | Some {perms={readonly=true}} -> pagefault addr
  | Some _ -> Machine.return {
      layers;
      values = Map.set values ~key:addr ~data:value;
    }

let add_layer layer t = {t with layers = layer :: t.layers}
let (++) = add_layer

let initialize addr_size values base len f =
  let m = Bitvec.modulus addr_size in
  Machine.Seq.fold (Seq.range 0 len) ~init:values ~f:(fun values i ->
      let addr = Bitvec.((base ++ i) mod m) in
      f addr >>= fun data ->
      Value.word data >>| fun data ->
      Map.set values ~key:addr ~data)



let allocate
    ?(executable=false)
    ?(readonly=false)
    ?init
    ?generator
    base len =
  memory >>= fun {addr_size; data_size=size} ->
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
    initialize addr_size s.values base len f >>= fun values ->
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

let get addr = with_curr @@ read addr

let set addr value =
  with_curr (write addr value) >>= put_curr

let load addr = get addr >>| Value.to_word
let store addr value = Value.word value >>= set addr

let is_mapped addr = with_curr @@ fun {addr_size=size} {layers} ->
  Machine.return @@
  Option.is_some (find_layer addr size layers)

let is_writable addr =
  with_curr @@ fun {addr_size} {layers} ->
  Machine.return @@begin
    find_layer addr addr_size layers |>
    function Some {perms={readonly}} -> not readonly
           | None -> false
  end
