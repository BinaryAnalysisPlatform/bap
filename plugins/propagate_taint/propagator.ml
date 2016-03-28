open Core_kernel.Std
open Bap.Std
open Microx.Std
open Format
module SM = Monad.State
open SM.Monad_infix

type taints = Tid.Set.t Var.Map.t Tid.Map.t

class type result = object
  method visited : int Tid.Map.t
  method tainted_regs : tid -> Taint.map
  method tainted_ptrs : tid -> Taint.map
end

let propagate taints vars tid v r : taints =
  let ts = taints r in
  Map.change vars tid (function
      | None when Set.is_empty ts -> None
      | None -> Some (Var.Map.of_alist_exn [v, ts])
      | Some vs -> Option.some @@ Map.change vs v (function
          | None when Set.is_empty ts -> None
          | None -> Some ts
          | Some ts' -> Some (Set.union ts ts')))

let taints_of_tid taints tid =
  Map.find taints tid |> function
  | None -> Var.Map.empty
  | Some ts -> ts

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

  method tainted_regs = taints_of_tid tvs
  method tainted_ptrs = taints_of_tid tms

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

let taint_term t ctxt v =
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

class ['a] main ?deterministic ?random_seed ?reg_policy ?mem_policy proj =
  let prog = Project.program proj in
  let memory = memory_lookup proj in
  let lookup = register_lookup proj in
  object(self)
    constraint 'a = #context
    inherit ['a] Conqueror.main ?deterministic prog as super
    inherit ['a] Concretizer.main ~memory ~lookup
        ?random_seed ?reg_policy ?mem_policy () as concrete
    inherit ['a] Taint.propagator

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
      if is_seeded def then begin
        super#eval_def def >>= fun () ->
        super#lookup (Def.lhs def) >>= fun x ->
        SM.get () >>= fun ctxt ->
        SM.put (taint_term def ctxt x)
      end else super#eval_def def
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
  (Monad.State.exec res ctxt :> result)
