open Core_kernel
open Bap.Std
open Format

open Bap_primus_types

module Generator = Bap_primus_generator
open Bap_primus_sexp

type exn += Pagefault of addr

let () = Exn.add_printer (function
    | Pagefault here ->
      Some (asprintf "Page fault at %a"
              Addr.pp_hex here)
    | _ -> None)

type dynamic = {
  lower : addr;
  upper : addr;
  value : Generator.t
}

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
  let addr_size d = d.addr
  let data_size d = d.size
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

let sexp_of_word w = Sexp.Atom (asprintf "%a" Word.pp_hex w)

let sexp_of_dynamic {lower; upper; value} =
  Sexp.(List [
      Sexp.Atom "dynamic";
      sexp_of_word lower;
      sexp_of_word upper;
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

let inspect_generated (ptr,x) = Sexp.List [
    sexp_of_addr ptr;
    sexp_of_value x;
  ]


let generated,on_generated =
  Bap_primus_observation.provide "load-generated"
    ~inspect:inspect_generated
    ~package:"bap"
    ~desc:"Occurs when a new value is generated during the load operation"


let virtual_memory arch =
  let module Target = (val target_of_arch arch) in
  let mem = Target.CPU.mem in
  match Var.typ mem with
  | Type.Imm _ | Type.Unk as t ->
    invalid_argf "The CPU.mem variable %a:%a is not a storage"
      Var.pps mem Type.pps t ()
  | Type.Mem (ks,vs) ->
    let ks = Size.in_bits ks and vs = Size.in_bits vs in
    Descriptor.create ks vs (Var.name mem)


let state = Bap_primus_machine.State.declare
    ~uuid:"4b94186d-3ae9-48e0-8a93-8c83c747bdbb"
    ~inspect:inspect_memory
    ~name:"memory" @@ fun p -> {
    mems = Descriptor.Map.empty;
    curr = virtual_memory (Project.arch p);
  }

let inside {lower; upper} addr =
  if Addr.(lower <= upper)
  then Addr.(addr >= lower) && Addr.(addr <= upper)
  else Addr.(addr >= upper) || Addr.(addr <= lower)

let find_layer addr = List.find ~f:(function
    | {mem=Dynamic mem} -> inside mem addr
    | {mem=Static  mem} -> Memory.contains mem addr)

let is_mapped addr {layers; values} =
  Map.mem values addr ||
  Option.is_some @@ find_layer addr layers

let empty_state = {
  values = Addr.Map.empty;
  layers = [];
}

module Make(Machine : Machine) = struct
  open Machine.Syntax

  module Generate = Generator.Make(Machine)
  module Value = Bap_primus_value.Make(Machine)

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
      mems = Map.set mems ~key:curr ~data:mem
    }

  let update f =
    get_curr >>= fun s ->
    put_curr (f s)

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

  let store_word arch bytesize ~addr word values =
    let bytes = (Word.bitwidth word + bytesize - 1) / bytesize in
    let bite x = Word.extract_exn ~hi:(bytesize-1) x in
    let shift = let bits = Word.of_int ~width:8 bytesize in
      fun x -> Word.lshift x bits in
    let addr,next = match Arch.endian arch with
      | LittleEndian -> addr,Addr.succ
      | BigEndian -> Addr.nsucc addr (bytes-1),Addr.pred in
    let notify_if_generated key x values =
      if not (Map.mem values key)
      then Machine.Observation.make on_generated (key,x)
      else Machine.return () in
    let rec loop written ~key word values =
      if written < bytes then
        Value.of_word (bite word) >>= fun data ->
        notify_if_generated key data values >>= fun () ->
        loop
          (written+1)
          ~key:(next key)
          (shift word)
          (Map.update values key ~f:(function
               | None -> data
               | Some data -> data))
      else Machine.return values in
    loop 0 ~key:addr word values

  let remembered {values; layers} addr word =
    memory >>= fun {size} ->
    Machine.gets Project.arch >>= fun arch ->
    store_word arch size addr word values >>= fun values ->
    put_curr {layers; values} >>| fun () ->
    Map.find_exn values addr

  let read addr {values;layers} = match Map.find values addr with
    | Some v -> Machine.return v
    | None -> match find_layer addr layers with
      | None -> pagefault addr
      | Some layer ->
        memory >>= fun {size} ->
        match layer.mem with
        | Static mem -> Value.of_word (read_word mem addr size)
        | Dynamic {value=g} ->
          Generate.word g (Generator.width g) >>=
          remembered {values; layers} addr

  let set_value s addr value = {
    s with
    values = Map.set s.values ~key:addr ~data:value
  }

  let write addr value s =
    if Map.mem s.values addr
    then Machine.return @@ set_value s addr value
    else match find_layer addr s.layers with
      | None -> pagefault addr
      | Some {perms={readonly=true}} -> pagefault addr
      | Some _ ->
        Machine.return @@ set_value s addr value

  let add_layer layer t = {t with layers = layer :: t.layers}
  let (++) = add_layer

  let initialize values lower upper f =
    let rec loop values addr =
      if Addr.(addr < upper)
      then f addr >>= fun data ->
        Value.of_word data >>= fun data ->
        loop (Map.set values ~key:addr ~data) (Addr.succ addr)
      else Machine.return values in
    loop values lower

  let add_region
      ?(readonly=false)
      ?(executable=false)
      ?init
      ?generator
      ~lower ~upper () =
    Machine.gets Project.arch >>= fun arch ->
    let width = Arch.addr_size arch |> Size.in_bits in
    get_curr >>| add_layer {
      perms={readonly; executable};
      mem = Dynamic {lower;upper; value = match generator with
          | Some g -> g
          | None -> Generator.Random.Seeded.lcg ~width ()}
    } >>= fun s ->
    match init with
    | None -> put_curr s
    | Some f ->
      initialize s.values lower upper f >>= fun values ->
      put_curr {s with values}

  let allocate
      ?readonly ?executable ?init ?generator base len =
    add_region ()
      ?readonly ?executable ?init ?generator
      ~lower:base
      ~upper:(Addr.nsucc base (len-1))

  let map ?(readonly=false) ?(executable=false) mem =
    update @@ add_layer ({mem=Static mem; perms={readonly; executable}})
  let add_text mem = map mem ~readonly:true  ~executable:true
  let add_data mem = map mem ~readonly:false ~executable:false

  let get addr = get_curr >>= read addr


  let set addr value =
    get_curr >>=
    write addr value >>=
    put_curr

  let del addr = update @@ fun s -> {
      s with values = Map.remove s.values addr
    }

  let load addr = get addr >>| Value.to_word
  let store addr value = Value.of_word value >>= set addr

  let is_mapped addr =
    get_curr >>| is_mapped addr

  let is_writable addr =
    get_curr >>| fun {layers; values} ->
    Map.mem values addr ||
    find_layer addr layers |>
    function Some {perms={readonly}} -> not readonly
           | None -> false
end
