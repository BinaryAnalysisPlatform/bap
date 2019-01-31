open Core_kernel
open Regular.Std
open Bap.Std

[@@@warning "-D"]
open Microx.Std
open Monads.Std

open Format
module SM = Monad.State
open SM.Let_syntax
open SM.Monad_infix

type taints = Tid.Set.t Var.Map.t Tid.Map.t
  [@@deriving bin_io, compare, sexp]

module Result = struct
  type t = {
    visited : int Tid.Map.t;
    tainted_regs : taints;
    tainted_ptrs : taints;
  } [@@deriving bin_io, compare, sexp]


  let empty = {
    visited = Tid.Map.empty;
    tainted_regs = Tid.Map.empty;
    tainted_ptrs = Tid.Map.empty;
  }

  let of_context ctxt = {
    visited = ctxt#visited;
    tainted_regs = ctxt#tvs;
    tainted_ptrs = ctxt#tms;
  }

  let taints_of_tid taints tid : Taint.map =
    Map.find taints tid |> function
    | None -> Var.Map.empty
    | Some ts -> ts

  let tainted_regs t = taints_of_tid t.tainted_regs
  let tainted_ptrs t = taints_of_tid t.tainted_ptrs
  let visited t = t.visited

  let is_tainted t tid =
    Map.mem t.tainted_regs tid || Map.mem t.tainted_ptrs tid

  let union_maps x y ~f =
    Map.merge x y ~f:(fun ~key -> function
        | `Left s | `Right s -> Some s
        | `Both (s1,s2) -> Some (f s1 s2))

  let union_taints : taints -> taints -> taints =
    union_maps ~f:(union_maps ~f:Set.union)

  let is_visited t tid = Map.mem t.visited tid

  let union x y = {
    visited = union_maps ~f:max x.visited y.visited;
    tainted_regs = union_taints x.tainted_regs y.tainted_regs;
    tainted_ptrs = union_taints x.tainted_ptrs y.tainted_ptrs;
  }

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let module_name = None
      let version = "1.0.0"
      let hash = Hashtbl.hash
      let pp ppf t = Format.fprintf ppf "<taints>"
    end)
end

type result = Result.t
  [@@deriving bin_io, compare, sexp]

let propagate taints vars tid v r : taints =
  let ts = taints r in
  Map.change vars tid (function
      | None when Set.is_empty ts -> None
      | None -> Some (Var.Map.of_alist_exn [v, ts])
      | Some vs -> Option.some @@ Map.change vs v (function
          | None when Set.is_empty ts -> None
          | None -> Some ts
          | Some ts' -> Some (Set.union ts ts')))


class context ?max_steps ?max_loop p  = object(self : 's)
  inherit Taint.context as taints
  inherit Conqueror.context ?max_steps ?max_loop p as super

  val tvs : taints = Tid.Map.empty
  val tms : taints = Tid.Map.empty

  method tvs = tvs
  method tms = tms

  method propagate_var tid v r =
    {< tvs = propagate taints#reg_taints tvs tid v r >}

  method propagate_mem tid v r : 's =
    match Bil.Result.value r with
    | Bil.Bot | Bil.Mem _ -> self
    | Bil.Imm a ->
      {< tms = propagate taints#ptr_taints tms tid v a >}


  method! merge runner =
    let self = super#merge runner in
    self#merge_taints runner

  method merge_taints runner =
    {< tvs = runner#tvs; tms = runner#tms >}

end

let taint_reg ctxt x seed =
  ctxt#taint_reg x (Tid.Set.singleton seed)

let taint_ptr ctxt x seed =
  match Bil.Result.value x with
  | Bil.Mem _ | Bil.Bot -> ctxt
  | Bil.Imm a ->
    ctxt#taint_ptr a `r8 (Tid.Set.singleton seed)

let taint_result t ctxt v =
  match Term.get_attr t Taint.reg, Term.get_attr t Taint.ptr with
  | None,None -> ctxt
  | None,Some seed -> taint_ptr ctxt v seed
  | Some seed,None -> taint_reg ctxt v seed
  | Some x, Some y -> taint_ptr (taint_reg ctxt v x) v y

let is_seeded t =
  Term.has_attr t Taint.reg || Term.has_attr t Taint.ptr

let memory_lookup proj addr =
  let memory = Project.memory proj in
  Memmap.lookup memory addr |> Seq.hd |> function
  | None -> None
  | Some (mem,_) -> match Memory.get ~addr mem with
    | Ok w -> Some w
    | _ -> None

let register_lookup proj =
  let arch = Project.arch proj in
  let width = Arch.addr_size arch |> Size.in_bits in
  let mem_start = Word.of_int64 ~width 0x40000000L in
  let module Target = (val target_of_arch arch) in
  fun var -> Option.some_if (Target.CPU.is_sp var) mem_start

let foreach seq ~f =
  SM.get () >>= fun ctxt ->
  Seq.fold seq ~init:(SM.return ()) ~f:(fun m x ->
      m >>= fun () -> f x) >>= fun () ->
  SM.return ()

class ['a] main ?deterministic ?random_seed ?reg_policy ?mem_policy proj =
  let prog = Project.program proj in
  let memory = memory_lookup proj in
  let lookup = register_lookup proj in
  object(self)
    constraint 'a = #context
    inherit ['a] Conqueror.main ?deterministic prog as super
    inherit! ['a] Concretizer.main ~memory ~lookup
        ?random_seed ?reg_policy ?mem_policy () as concrete
    inherit! ['a] Taint.propagator

    method! empty = new Bil.Storage.sparse

    method! lookup v =
      concrete#lookup v >>= fun r ->
      SM.get () >>= fun ctxt ->
      match List.hd ctxt#trace with
      | None -> SM.return r
      | Some tid ->
        let ctxt = ctxt#propagate_var tid v r in
        let ctxt = ctxt#propagate_mem tid v r in
        SM.put ctxt >>= fun () ->
        SM.return r

    method! eval_def def =
      self#taint_free_vars def >>= fun () ->
      super#eval_def def

    method private taint_free_vars def =
      if is_seeded def
      then foreach (Set.to_sequence (Def.free_vars def)) ~f:(fun var ->
          self#lookup var >>= fun r ->
          self#update var r >>= fun () ->
          SM.update (fun ctxt -> taint_result def ctxt r))
      else SM.return ()
  end


exception Entry_point_not_found

let run_from_tid p (biri : 'a #main) tid =
  match Program.lookup sub_t p tid with
  | Some sub -> biri#eval_sub sub
  | None -> raise Entry_point_not_found

let tid_of_name str =
  match Tid.from_string ("@"^str) with
  | Ok tid -> tid
  | Error _ -> raise Entry_point_not_found

let tid_of_ident = function
  | `Term tid -> tid
  | `Name str -> tid_of_name str

let run_from_point p biri point =
  run_from_tid p biri (tid_of_ident point)

let run
    ~max_steps ~max_loop ~deterministic
    ?random_seed ~reg_policy ~mem_policy proj point =
  let p = Project.program proj in
  let ctxt = new context ~max_steps ~max_loop p in
  let biri = new main ~deterministic ?random_seed ~reg_policy ~mem_policy proj in
  let res = run_from_point p biri point in
  (Monad.State.exec res ctxt |> Result.of_context)
