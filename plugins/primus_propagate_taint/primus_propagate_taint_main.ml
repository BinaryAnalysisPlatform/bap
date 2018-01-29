open Core_kernel.Std
open Regular.Std
open Bap.Std
open Bap_primus.Std
open Monads.Std
open Format
include Self()

module Vid = Primus.Value.Id
type vid = Vid.t

(* v[tid] -> taints *)
type taints = Tid.Set.t Var.Map.t Tid.Map.t

(** how we are referencing a tainted object  *)
type reference_kind =
  | Ptr           (* by a pointer *)
  | Reg                         (* directly *)
[@@deriving bin_io, compare, sexp]

type coeff = {
  reads : Primus.value list;
  loads : Primus.value list;
}


type objects = Tid.Set.t

(* state of the taint engine.
 *
 * The taint engine maintains a set of values and pointers that
 * reference to tainted objects.
 *
 * Each object is represented by a unique id, that was assigned to it,
 * when the object was originally tainted (i.e., when the taint was introduced).
 *
 * An object could be referenced directly, i.e., it could be a result
 * of a computation (value), represnted by the direct mapping. Or an
 * object could be stored in dymanic memory at the specified address(es).
 *
 * If a tainted object is no longer reachable, then we say that the
 * taint is killed. Thus precise tainting is an instance of the
 * garbage collection problem.
 *
*)
type tainter = {
  direct : objects Vid.Map.t;
  indirect : objects Addr.Map.t;
} [@@deriving fields]

(* generalized access to the tainter fields
 *
 * - [select.field] selects either direct or indirect field of the
 *   tainter (should be used with [Field.get] and [Field.fset])
 *  - [select.key] maps value to the key of the appropriate type
 *   (i.e., to Id in case of direct, and addr in case of indirect)
 *
 *  With the selector object it is possible to write a function that
 *  will be polymorphic across two fields of the tainter (sort of
 *  extremly boundned polymorphism).
*)
type selector = Field : {
    field : (tainter, ('k,objects,'c) Map.t) Field.t;
    key : (Primus.value -> 'k);
  } -> selector


type mapper = {
  regs : taints;
  ptrs : taints;
} [@@deriving fields]

type intro = {
  coeff : coeff option;
}


type gc_local = {
  old : tainter;
}

let empty_tainter = {
  direct = Vid.Map.empty;
  indirect = Addr.Map.empty;
}

let nocoeff = {reads = []; loads = []}

let tainter = Primus.Machine.State.declare
    ~name:"primus-tainter"
    ~uuid:"2d4a4208-f918-4cf7-8e1b-5d8400a106d3"
    (fun _ -> empty_tainter)

let mapper = Primus.Machine.State.declare
    ~name:"primus-taint-mapper"
    ~uuid:"6e11c845-fee9-4d8c-898e-6aa1750718ee"
    (fun _ ->  {
         regs = Tid.Map.empty;
         ptrs = Tid.Map.empty;
       })

let gc = Primus.Machine.State.declare
    ~name:"primus-taint-gc"
    ~uuid:"2357826e-d5b7-40a3-8f90-0cfd7b48eadc"
    (fun _ -> {old = empty_tainter})


let intro = Primus.Machine.State.declare
    ~name:"primus-taint-introducer"
    ~uuid:"fd12a09a-57bf-4b5c-a9b7-a0e27768f5c9"
    (fun _ -> {coeff = None})

let taint_introduced,introduced_taint =
  Primus.Observation.provide
    ~inspect:sexp_of_tid "taint-introduced"

let taint_propagated,propagated_taint =
  Primus.Observation.provide
    ~inspect:sexp_of_tid "taint-propagated"

let taint_killed,killed_taint =
  Primus.Observation.provide
    ~inspect:sexp_of_tid "taint-killed"

let vid = Primus.Value.id

let indirect = Field {
    field = Fields_of_tainter.indirect;
    key = Primus.Value.to_word
  }
let direct = Field {
    field = Fields_of_tainter.direct;
    key = vid;
  }


module Intro(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Eval = Primus.Interpreter.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  let introduces def =
    Term.has_attr def Taint.reg || Term.has_attr def Taint.ptr

  let start_observing_coeff def =
    if introduces def
    then Machine.Local.put intro {coeff = Some nocoeff}
    else Machine.return ()

  let stop_observing_coeff def =
    if introduces def
    then Machine.Local.put intro {coeff = None}
    else Machine.return ()

  (* taint kind taint v - taints value v with the given taint *)
  let taint (Field dst) taint v =
    Machine.Local.get tainter >>= fun s ->
    Map.update (Field.get dst.field s) (dst.key v) ~f:(function
        | None -> Tid.Set.singleton taint
        | Some taints -> Set.add taints taint) |>
    Field.fset dst.field s |>
    Machine.Local.put tainter

  let taint_var kinds v =
    Machine.List.iter kinds ~f:(function
        | (Reg,t) -> taint direct t v
        | (Ptr,t) -> taint indirect t v)

  let constant (_v : Primus.value) _kinds = Machine.return ()

  (* a pointer to the tainted object  *)
  let taint_direct_addr t v = taint indirect t v

  let min_addr addrs =
    match List.min_elt addrs ~cmp:Value.compare with
    | None -> assert false
    | Some x -> x

  (* a pointer to a pointer to the tainted object *)
  let taint_indirect_addr t addrs =
    let addr = min_addr addrs in
    Machine.arch >>= fun arch ->
    let size = (Arch.addr_size arch :> size) in
    Eval.load addr (Arch.endian arch) size >>=
    taint_direct_addr t

  let loaded addrs v kinds =
    taint_var kinds v >>= fun () ->
    Machine.List.iter kinds ~f:(function
        | (Reg,t) -> taint_direct_addr t v
        | (Ptr,t) -> taint_indirect_addr t addrs)

  let read vids v kinds =
    taint_var kinds v >>= fun () ->
    Machine.List.iter vids ~f:(fun v ->
        taint_var kinds v)

  let get_attr kind attr def = match Term.get_attr def attr with
    | None -> None
    | Some t -> Some (kind,t)

  let introduce f def =
    let kinds = List.filter_opt [
        get_attr Ptr Taint.ptr def;
        get_attr Reg Taint.reg def;
      ] in
    Eval.get (Def.lhs def) >>= fun v ->
    taint_var kinds v >>= fun () ->
    f v kinds

  let introduce_taints t =
    Machine.Local.get intro >>= function
    | {coeff=None} -> Machine.return ()
    | {coeff=Some {loads=[]; reads=[]}} -> introduce constant t
    | {coeff=Some {loads=[]; reads=xs}} -> introduce (read xs) t
    | {coeff=Some {loads=xs}} -> introduce (loaded xs) t

  let enter_def = start_observing_coeff

  let leave_def d = Machine.sequence [
      introduce_taints d;
      stop_observing_coeff d;
    ]

  let init () = Machine.sequence Primus.[
      Interpreter.enter_def >>> enter_def;
      Interpreter.leave_def >>> leave_def;
    ]
end

module Propagate(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Eval = Primus.Interpreter.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  let union_taints (Field ms) srcs s =
    List.fold srcs ~init:Tid.Set.empty ~f:(fun objs src ->
        match Map.find (Field.get ms.field s) (ms.key src) with
        | None -> objs
        | Some objs' -> Set.union objs' objs)

  let add_taints m key set =
    if Set.is_empty set
    then Map.remove m key
    else Map.add m ~key ~data:set

  (** [ms --> md] transfers references from the [ms] mapping to the
      [md] mapping.  *)
  let (-->) ms (Field md) srcs dst =
    Machine.Local.update tainter ~f:(fun s ->
        union_taints ms srcs s |>
        add_taints (Field.get md.field s) (md.key dst) |>
        Field.fset md.field s)

  let one f (x,y) = f [x] y
  let loaded = one (indirect --> direct)
  let stored = one (direct --> indirect)
  let computed = one (direct --> direct)
  let binop ((_op,x,y),r) = (direct --> direct) [x;y] r
  let unop ((_op,x),r) = computed (x,r)
  let extract ((_,_,x),r) = computed (x,r)
  let cast ((_,_,x),r) = computed (x,r)

  let init () = Machine.sequence Primus.[
      Interpreter.loaded  >>> loaded;
      Interpreter.stored  >>> stored;
      Interpreter.binop   >>> binop;
      Interpreter.unop    >>> unop;
      Interpreter.extract >>> extract;
      Interpreter.cast    >>> cast;
    ]
end


(* tracks live tainted objects, removes unecessary references.

   A tainted object is live, if it is reachable either directly or
   indirectly in the tainted. The indirect references are dropped
   implicitly, when an address is overwritten with the new tainted
   object (since we have finite set of addresses). The direct
   references use value identifiers, that are dynamic, so we need to
   track which of them still live. Since values are only stoted in
   Env, we can easily compute the set of live values.

   Finally, we compute a set of live objects on the entrance to a
   basic block, and a set of live objects at the exit of the basic
   block, the difference between these two are the objects that were
   killed in the block. We make an observation about each killed
   object.

*)
module Gc(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Env = Primus.Env.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  (* removes directly dead taints *)
  let collect_direct =
    Machine.Local.get tainter >>= fun s ->
    Env.all >>= fun vars ->
    Machine.Seq.fold vars ~init:Vid.Set.empty ~f:(fun live var ->
        Env.get var >>| fun v ->
        Set.add live (Value.id v)) >>= fun live ->
    Machine.Local.put tainter {
      s with direct = Map.filter_keys s.direct ~f:(Set.mem live)
    }

  (* a set of live tainted objects *)
  let objects {direct; indirect} : objects =
    let to_set m : objects =
      Map.to_sequence m |>
      Seq.map ~f:snd |>
      Seq.fold ~init:Tid.Set.empty ~f:Set.union in
    Set.union (to_set direct) (to_set indirect)

  let main _ =
    Machine.Local.get gc >>= fun {old} ->
    collect_direct >>= fun () ->
    Machine.Local.get tainter >>= fun cur ->
    let dead = Set.diff (objects old) (objects cur) in
    Set.to_sequence dead |> Machine.Seq.iter
      ~f:(Machine.Observation.make killed_taint) >>= fun () ->
    Machine.Local.put gc {old = cur}


  let init () = Primus.Interpreter.leave_blk >>> main

end


module Mapper (Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Eval = Primus.Interpreter.Make(Machine)

  let update (Field src) dst (var,x) =
    Machine.Local.get tainter >>= fun tainter ->
    match Map.find (Field.get src.field tainter) (src.key x) with
    | None -> Machine.return ()
    | Some taints ->
      Eval.pos >>| Primus.Pos.tid >>= fun term ->
      Machine.Local.update mapper ~f:(fun m ->
          Map.update (Field.get dst m) term ~f:(function
              | None -> Var.Map.singleton var taints
              | Some vars -> Map.update vars var ~f:(function
                  | None -> taints
                  | Some taints' -> Set.union taints taints')) |>
          Field.fset dst m)

  let variable_read assn = Machine.sequence [
      update direct Fields_of_mapper.regs assn;
      update indirect Fields_of_mapper.ptrs assn;
    ]

  let init () =
    Primus.Interpreter.read >>> variable_read

end

module Marker(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Eval = Primus.Interpreter.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  let mark tag taints t = match Map.find taints (Term.tid t) with
    | Some ts when not(Map.is_empty ts) -> Term.set_attr t tag ts
    | _ -> t

  let mark_terms {regs; ptrs} = (object
    inherit Term.mapper as super
    method! map_term cls t =
      super#map_term cls t |>
      mark Taint.regs regs |>
      mark Taint.ptrs ptrs
  end)#run

  let mark _ =
    Machine.Local.get mapper >>= fun s ->
    Machine.update (fun proj ->
        Project.program proj |>
        mark_terms s |>
        Project.with_program proj)

  let init () =
    Primus.Interpreter.leave_blk >>> mark
end

let enable modules =
  List.iter ~f:Primus.Machine.add_component modules

open Config;;
manpage [
  `S "DESCRIPTION";
  `P "The Primus taint propagatation engine.";
]

let enabled = flag "run" ~doc:"Run taint propagation."
let don't_mark = flag "no-marks" ~doc:"Don't mark project terms"

let main : Primus.component list = [
  (module Intro);
  (module Propagate);
  (module Gc)
]

let markers : Primus.component list = [
  (module Mapper);
  (module Marker);
]

let () = when_ready (fun {get=(!!)} ->
    if !!enabled then enable main;
    if not !!don't_mark then enable markers)
