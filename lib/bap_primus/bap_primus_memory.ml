open Core_kernel.Std
open Bap.Std
open Format

open Bap_primus_types

module Observation = Bap_primus_observation
module Iterator = Bap_primus_iterator
module Random  = Bap_primus_random
module Generator = Bap_primus_generator
open Bap_primus_sexp

type exn += Segmentation_fault of addr

let sexp_of_segmentation_fault = sexp_of_word

let segmentation_fault, segfault =
  Observation.provide ~inspect:sexp_of_segmentation_fault
    "segmentation-fault"

let address_access,address_will_be_read =
  Observation.provide ~inspect:sexp_of_word "address-access"

let address_read,address_was_read =
  Observation.provide ~inspect:sexp_of_move "address-read"

let address_written,address_was_written =
  Observation.provide ~inspect:sexp_of_move
    "address-written"

let () = Exn.add_printer (function
    | Segmentation_fault here ->
      Some (sprintf "Segmentation fault at %s"
              (Addr.string_of_value here))
    | _ -> None)

type dynamic = {base : addr; len : int; value : Generator.t }
type region =
  | Dynamic of dynamic
  | Static  of mem

type perms = {readonly : bool; executable : bool}
type layer = {mem : region; perms : perms}

type t = {
  values : word Addr.Map.t;
  layers : layer list;
}

let zero = Word.of_int ~width:8 0

let sexp_of_word w = Sexp.Atom (Word.string_of_value w)

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

let inspect_memory {values; layers} =
  let values =
    Map.to_sequence values |> Seq.map ~f:(fun (key,value) ->
        Sexp.(List [sexp_of_word key; sexp_of_byte value])) |>
    Seq.to_list_rev in
  let layers = List.map layers ~f:(sexp_of_layer) in
  Sexp.(List [
      List [Atom "values"; List values];
      List [Atom "layers"; List layers];
    ])

let state = Bap_primus_machine.State.declare
    ~uuid:"4b94186d-3ae9-48e0-8a93-8c83c747bdbb"
    ~inspect:inspect_memory
    ~name:"memory"
    (fun _ -> {values = Addr.Map.empty; layers = []})

let inside {base;len} addr =
  Addr.(addr >= base) && Addr.(addr < base ++ len)

let find_layer addr = List.find ~f:(function
    | {mem=Dynamic mem} -> inside mem addr
    | {mem=Static  mem} -> Memory.contains mem addr)

let is_mapped addr {layers} = find_layer addr layers <> None


module Make(Machine : Machine) = struct
  open Machine.Syntax

  module Generate = Generator.Make(Machine)
  let (!!) = Machine.Observation.make 

  let update state f =
    Machine.Local.get state >>= fun s ->
    Machine.Local.put state (f s)

  let segfault addr =
    !!segfault addr >>= fun () ->
    Machine.current () >>= fun id -> 
    Machine.raise (Segmentation_fault addr)

  let read addr {values;layers} = match find_layer addr layers with
    | None -> segfault addr
    | Some layer -> match Map.find values addr with
      | Some v -> !!address_was_read (addr,v) >>| fun () -> v
      | None -> match layer.mem with
        | Dynamic {value} -> 
          Generate.next value >>| Word.of_int ~width:8 >>= fun v ->
          !!address_was_read (addr,v) >>| fun () -> v
        | Static mem -> match Memory.get ~addr mem with
          | Ok v -> !!address_was_read (addr,v) >>| fun () -> v
          | Error _ -> failwith "Bap_primus.Memory.read"


  let write addr value {values;layers} = match find_layer addr layers with
    | None -> segfault addr
    | Some {perms={readonly=true}} -> segfault addr
    | Some _ -> Machine.return {
        layers;
        values = Map.add values ~key:addr ~data:value;
      }

  let add_layer layer t = {t with layers = layer :: t.layers}
  let (++) = add_layer

  let allocate
      ?(readonly=false)
      ?(executable=false)
      ?(generator=Generator.Random.Seeded.byte)
      base len =
    update state @@ add_layer {
      perms={readonly; executable};
      mem = Dynamic {base;len; value=generator}
    }

  let map ?(readonly=false) ?(executable=false) mem =
    update state @@ add_layer ({mem=Static mem; perms={readonly; executable}})
  let add_text mem = map mem ~readonly:true  ~executable:true
  let add_data mem = map mem ~readonly:false ~executable:false

  let load addr =
    !!address_will_be_read addr >>= fun () ->
    Machine.Local.get state >>= read addr

  let save addr value =
    Machine.Local.get state >>=
    write addr value >>=
    Machine.Local.put state >>= fun () ->
    !!address_was_written (addr,value)
end
