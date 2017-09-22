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



(** how are we referencing a tainted object  *)
type reference_kind =
  | Ptr           (* by a pointer *)
  | Reg                         (* directly *)
[@@deriving bin_io, compare, sexp]

type coeff = {
  reads : Primus.value list;
  loads : Primus.value list;
}

type objects = Tid.Set.t

(* we have a set of tainted objects and we map it onto two sets:
   set of addresses that we believe are pointing into objects,
   set of computations that we believe are resulting

   we track all references to a tainted object and all computations
*)
type tainter = {
  direct : objects Vid.Map.t;
  indirect : objects Addr.Map.t;
} [@@deriving fields]


type mapper = {
  regs : taints;
  ptrs : taints;
} [@@deriving fields]

type intro = {
  coeff : coeff option;
}

type ('f,'k) mapping = {
  field : 'f;
  key : 'k;
}

let nocoeff = {reads = []; loads = []}

let tainter = Primus.Machine.State.declare
    ~name:"primus-tainter"
    ~uuid:"2d4a4208-f918-4cf7-8e1b-5d8400a106d3"
    (fun _ -> {
         direct = Vid.Map.empty;
         indirect = Addr.Map.empty;
       })

let mapper = Primus.Machine.State.declare
    ~name:"primus-taint-mapper"
    ~uuid:"6e11c845-fee9-4d8c-898e-6aa1750718ee"
    (fun _ ->  {
         regs = Tid.Map.empty;
         ptrs = Tid.Map.empty;
       })

let intro = Primus.Machine.State.declare
    ~name:"primus-taint-introducer"
    ~uuid:"fd12a09a-57bf-4b5c-a9b7-a0e27768f5c9"
    (fun _ -> {coeff = None})

let reg_taint_introduced,introduce_reg_taint =
  Primus.Observation.provide
    ~inspect:sexp_of_tid "introduce-reg-taint"

let reg_taint_propagated,propagate_reg_taint =
  Primus.Observation.provide
    ~inspect:sexp_of_tid "propagate-reg-taint"

let reg_taint_killed,kill_reg_taint =
  Primus.Observation.provide
    ~inspect:sexp_of_tid "kill-reg-taint"

let ptr_taint_introduced,introduce_ptr_taint =
  Primus.Observation.provide
    ~inspect:sexp_of_tid "introduce-ptr-taint"

let ptr_taint_propagated,propagate_ptr_taint =
  Primus.Observation.provide
    ~inspect:sexp_of_tid "propagate-ptr-taint"

let ptr_taint_killed,kill_ptr_taint =
  Primus.Observation.provide
    ~inspect:sexp_of_tid "kill-ptr-taint"

let vid = Primus.Value.id

let indirect = {
  field = Fields_of_tainter.indirect;
  key = Primus.Value.to_word
}
let direct = {
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
    then Machine.Local.update intro ~f:(fun s -> {
          coeff = Some nocoeff
        })
    else Machine.return ()

  let stop_observing_coeff def =
    if introduces def
    then Machine.Local.put intro {coeff = None}
    else Machine.return ()

  let taint dst taint v =
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

  let constant v kinds = Machine.return ()


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

  (** [ms --> md] transfers references from the [ms] mapping to the
      [md] mapping.  *)
  let (-->) ms md (src,dst) =
    Machine.Local.get tainter >>= fun s ->
    match Map.find (Field.get ms.field s) (ms.key src) with
    | None -> Machine.return ()
    | Some objs ->
      Set.fold objs ~init:(Field.get md.field s) ~f:(fun refs obj ->
          Map.update refs (md.key dst) ~f:(function
              | None -> Tid.Set.singleton obj
              | Some objs -> Set.add objs obj)) |>
      Field.fset md.field s |>
      Machine.Local.put tainter

  let loaded = indirect --> direct
  let stored = direct --> indirect
  let computed = direct --> direct

  let binop ((_op,x,y),r) = Machine.sequence [
      computed (x,r);
      computed (y,r);
    ]

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


module Mapper(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Eval = Primus.Interpreter.Make(Machine)

  let update src dst (var,x) =
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
                  | Some taints' -> Set.union taints taints)) |>
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
]

let markers : Primus.component list = [
  (module Mapper);
  (module Marker);
]

let () = when_ready (fun {get=(!!)} ->
    if !!enabled then enable main;
    if not !!don't_mark then enable markers)
