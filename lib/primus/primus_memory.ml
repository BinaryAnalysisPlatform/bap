open Core_kernel.Std
open Bap.Std

open Primus_types

module Context = Primus_context
module Observation = Primus_observation
module Iterator = Primus_iterator
module Random  = Primus_random
module Generator = Primus_generator


type error += Segmentation_fault of addr
type error += Stack_overflow of addr

let sexp_of_segmentation_fault addr =
  Sexp.List [Sexp.Atom "Segmentation fault"; sexp_of_addr addr]


let segmentation_fault, segfault =
  Observation.provide ~inspect:sexp_of_segmentation_fault
    "segmentation-fault"


module type S = sig
  type t
  type ('a,'e) m

  module Generator : Generator.S with type ('a,'e) m := ('a,'e) m

  val load : addr -> (word,#Context.t) m
  val save : addr -> word -> (unit,#Context.t) m

  val add_text : mem -> (unit,#Context.t) m
  val add_data : mem -> (unit,#Context.t) m

  val allocate :
    ?readonly:bool ->
    ?executable:bool ->
    ?policy:Generator.policy -> addr -> int -> (unit,#Context.t) m

end

module Make(Machine : Machine)
  : S with type ('a,'e) m := ('a,'e) Machine.t  = struct
  open Machine.Syntax

  module Generator = Primus_generator.Make(Machine)

  type ('a,'e) m = ('a,'e) Machine.t
  type 'a memory = {base : addr; len : int; value : 'a }
  type 'a region = {mem : 'a; readonly : bool; executable : bool}
  type constant = word
  type random   = Generator.t
  type mapped   = mem
  type layer =
    | Static of constant memory region
    | Random of random memory region
    | Memory of mapped region

  type t = {
    values : word Addr.Map.t;
    layers : layer list;
  }


  let zero = Word.of_int ~width:8 0

  let sexp_of_t = sexp_of_opaque
  let state = Machine.Local.create ~inspect:sexp_of_t ~name:"memory"
      (fun _ -> {values = Addr.Map.empty; layers = []})

  let inside {base;len} addr =
    Addr.(addr >= base) && Addr.(base ++ len < addr)

  let update state f =
    Machine.Local.get state >>= fun s ->
    Machine.Local.put state (f s)


  let find_layer addr = List.find ~f:(function
      | Static {mem} -> inside mem addr
      | Random {mem} -> inside mem addr
      | Memory {mem} -> Memory.contains mem addr)

  let segfault addr = Machine.fail (Segmentation_fault addr)

  let read addr {values;layers} = match find_layer addr layers with
    | None -> segfault addr
    | Some layer -> match Map.find values addr with
      | Some v -> Machine.return v
      | None -> match layer with
        | Static {mem={value}} -> Machine.return value
        | Random {mem={value}} -> Generator.next value >>| Word.of_int ~width:8
        | Memory {mem} -> match Memory.get ~addr mem with
          | Ok value -> Machine.return value
          | Error _ -> failwith "Primus.Memory.read"

  let is_mapped addr {layers} = find_layer addr layers <> None

  let is_readonly
      (Memory {readonly} | Random {readonly} | Static {readonly}) = readonly

  let write addr value {values;layers} = match find_layer addr layers with
    | None -> segfault addr
    | Some layer when is_readonly layer -> segfault addr
    | Some _ -> Machine.return {
        layers;
        values = Map.add values ~key:addr ~data:value;
      }

  let add_layer layer t = {t with layers = layer :: t.layers}
  let (++) = add_layer


  let set_stack ?(size=8*1024*1024) ?rng base =
    let rng = match rng with
      | Some rng -> Machine.return rng
      | None -> Generator.Seeded.byte () in
    rng >>= fun rng ->
    update state @@ add_layer (Random {
      mem = {base; len=size; value=rng};
      readonly = false;
      executable = false;
    })

  let allocate
      ?(readonly=true)
      ?(executable=true)
      ?(policy=`static zero)
      base len =

    let layer = match policy with
      | `static value -> Machine.return (Static {
          mem = {base;len;value};
          readonly; executable;
        })
      | `random (Some value) -> Machine.return (Random {
          mem = {base; len; value};
          readonly; executable
        })
      | `random None ->
        Generator.Seeded.byte () >>| fun value -> Random {
          mem = {base; len; value};
          readonly; executable
        } in
    layer >>= fun layer -> update state @@ add_layer layer

  let add_mem mem ~readonly ~executable =
    update state @@ add_layer (Memory {mem; readonly; executable})
  let add_text mem = add_mem mem ~readonly:true  ~executable:true
  let add_data mem = add_mem mem ~readonly:false ~executable:false

  let load addr =
    Machine.Local.get state >>= read addr

  let save addr value =
    Machine.Local.get state >>=
    write addr value >>=
    Machine.Local.put state
end
