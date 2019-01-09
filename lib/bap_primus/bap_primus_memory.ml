open Core_kernel.Std
open Bap.Std
open Format

open Bap_primus_types

module Observation = Bap_primus_observation
module Iterator = Bap_primus_iterator
module Random  = Bap_primus_random
module Generator = Bap_primus_generator
open Bap_primus_sexp

type exn += Pagefault of addr

let () = Exn.add_printer (function
    | Pagefault here ->
      Some (asprintf "Page fault at %a"
              Addr.pp_hex here)
    | _ -> None)

type dynamic = {base : addr; len : int; value : Generator.t }
type region =
  | Dynamic of dynamic
  | Static  of mem

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
  values : value Addr.Map.t;
  layers : layer list;
}

type t = {
  curr : Descriptor.t;
  mems : state Descriptor.Map.t
}

let zero = Word.of_int ~width:8 0

let sexp_of_word w = Sexp.Atom (asprintf "%a" Word.pp_hex w)

let sexp_of_dynamic {base; len; value} =
  Sexp.(List [
      Sexp.Atom "dynamic";
      sexp_of_word base;
      sexp_of_int len;
      Generator.sexp_of_t value])

let sexp_of_mem mem = Sexp.List [
    Sexp.Atom "static";
    sexp_of_word (Memory.min_addr mem);
    sexp_of_int (Memory.length mem);
  ]

let sexp_of_mem = function
  | Dynamic mem -> sexp_of_dynamic mem
  | Static  mem -> sexp_of_mem mem

let sexp_of_layer {mem; perms={readonly; executable}} =
  let flags = [
    "R";
    if readonly then "" else "W";
    if executable then "X" else "";
  ] |> String.concat ~sep:"" in
  Sexp.(List [sexp_of_mem mem; Atom flags])

let inspect_state {values; layers} =
  let values =
    Map.to_sequence values |> Seq.map ~f:(fun (key,value) ->
        Sexp.(List [sexp_of_word key; sexp_of_value value])) |>
    Seq.to_list_rev in
  let layers = List.map layers ~f:(sexp_of_layer) in
  Sexp.(List [
      List [Atom "values"; List values];
      List [Atom "layers"; List layers];
    ])

let inspect_memory {curr; mems} = Sexp.List [
    Sexp.Atom curr.name;
    Descriptor.Map.sexp_of_t inspect_state mems;
  ]

let state = Bap_primus_machine.State.declare
    ~uuid:"4b94186d-3ae9-48e0-8a93-8c83c747bdbb"
    ~inspect:inspect_memory
    ~name:"memory"
    (fun p ->
       let addr_size = Size.in_bits (Arch.addr_size (Project.arch p)) in {
         mems = Descriptor.Map.empty;
         curr = Descriptor.unknown addr_size 8;
       })

let inside {base;len} addr =
  let high = Word.(base ++ len) in
  if Addr.(high < base)
  then Addr.(addr >= base) || Addr.(addr < high)
  else Addr.(addr >= base) && Addr.(addr < high)

let find_layer addr = List.find ~f:(function
    | {mem=Dynamic mem} -> inside mem addr
    | {mem=Static  mem} -> Memory.contains mem addr)

let is_mapped addr {layers} = find_layer addr layers <> None

let empty_state = {
  values = Addr.Map.empty;
  layers = [];
}

module Make(Machine : Machine) = struct
  open Machine.Syntax

  module Generate = Generator.Make(Machine)
  module Value = Bap_primus_value.Make(Machine)
  let (!!) = Machine.Observation.make


  let memory =
    Machine.Local.get state >>| fun s -> s.curr

  let switch curr =
    Machine.Local.update state ~f:(fun s -> {s with curr})

  let get_curr =
    Machine.Local.get state >>| fun {curr; mems} ->
    match Map.find mems curr with
    | None -> empty_state
    | Some s -> s

  let put_curr mem =
    Machine.Local.get state >>= fun {curr; mems} ->
    Machine.Local.put state {
      curr;
      mems = Map.add mems ~key:curr ~data:mem
    }

  let update state f =
    Machine.Local.get state >>= fun s ->
    match Map.find s.mems s.curr with
    | None -> Machine.return ()
    | Some m -> Machine.Local.put state {
        s with
        mems = Map.add s.mems ~key:s.curr ~data:(f m)
      }

  let pagefault addr = Machine.raise (Pagefault addr)

  let read_small mem addr size =
    assert (size < 8 && size > 0);
    let addr_size = Addr.bitwidth addr in
    (* to address {n 2^m} bits we need {m+log(m)} addr space,
       since {n < 8} (it is the word size in bits), we just add 3.*)
    let width = addr_size + 3 in
    let addr = Word.extract_exn ~hi:(width-1) addr in
    let off = Addr.((addr - Memory.min_addr mem)) in
    let off_in_bits = Word.(off * Word.of_int ~width size) in
    let full_bytes = Word.(off_in_bits / Word.of_int ~width 8) in
    let bit_off =
      Word.to_int_exn @@
      Word.(off_in_bits - full_bytes * Word.of_int ~width 8) in
    let leftover = Memory.length mem - Word.to_int_exn full_bytes in
    let len = min leftover 2 in
    let full_bytes = Word.extract_exn ~hi:(addr_size-1) full_bytes in
    let from = Addr.(Memory.min_addr mem + full_bytes) in
    let mem = ok_exn @@ Memory.view mem ~from ~words:len in
    let data = Bigsubstring.to_string (Memory.to_buffer mem) in
    let vec = Word.of_binary ~width:(len * 8) BigEndian data in
    let hi = len * 8 - bit_off - 1 in
    let lo = hi - size + 1 in
    Word.extract_exn ~hi ~lo vec


  (* we can't use Bap.Std.Memory here as we need arbitrary lengths *)
  let read_word mem base size =
    let start,next = match Memory.endian mem with
      | LittleEndian -> Addr.nsucc base (size/8-1), Addr.pred
      | BigEndian -> base, Addr.succ in
    let rec read addr left =
      let data = ok_exn @@ Memory.get ~addr mem in
      if left <= 8
      then Word.extract_exn ~lo:(8-left) data
      else Word.concat data @@ read (next addr) (left - 8) in
    if size >= 8 then read start size
    else read_small mem base size

  let read addr {values;layers} = match find_layer addr layers with
    | None -> pagefault addr
    | Some layer -> match Map.find values addr with
      | Some v -> Machine.return v
      | None ->
        memory >>= fun {size} ->
        match layer.mem with
        | Dynamic {value} ->
          Generate.next value >>= Value.of_int ~width:8
        | Static mem -> Value.of_word (read_word mem addr size)

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
        let addr = Addr.(base ++ i) in
        f addr >>= fun data ->
        Value.of_word data >>| fun data ->
        Map.add values ~key:addr ~data)



  let allocate
      ?(readonly=false)
      ?(executable=false)
      ?init
      ?(generator=Generator.Random.Seeded.byte)
      base len =
    get_curr >>| add_layer {
      perms={readonly; executable};
      mem = Dynamic {base;len; value=generator}
    } >>= fun s ->
    match init with
    | None -> put_curr s
    | Some f ->
      initialize s.values base len f >>= fun values ->
      put_curr {s with values}

  let map ?(readonly=false) ?(executable=false) mem =
    update state @@ add_layer ({mem=Static mem; perms={readonly; executable}})
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
end
