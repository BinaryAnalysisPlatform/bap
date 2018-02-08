open Core_kernel.Std
open Bap.Std
open Monads.Std
open Bap_primus.Std
module Legacy_taint = Taint
open Bap_taint.Std

include Self()


(* Machine Identifier *)
module Mid = Monad.State.Multi.Id

(* v[tid] -> taints *)
type taints = Tid.Set.t Var.Map.t Tid.Map.t

type mapper = {
  regs : taints;
  ptrs : taints;
  tids : tid Taint.Object.Map.t;

} [@@deriving fields]

let mapper = Primus.Machine.State.declare
    ~name:"primus-taint-mapper"
    ~uuid:"12511d75-e626-4b0d-8dcd-2a3b597992cf"
    (fun _ ->  {
         regs = Tid.Map.empty;
         ptrs = Tid.Map.empty;
         tids = Taint.Object.Map.empty;
       })


module Intro(Machine : Primus.Machine.S) = struct
  module Env = Primus.Interpreter.Make(Machine)
  module Eval = Primus.Interpreter.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  module Tracker = Taint.Tracker(Machine)
  module Kind = Taint.Kind.Make(Machine)
  module Object = Taint.Object.Make(Machine)

  open Machine.Syntax


  let kind t =
    Kind.create (sprintf "user-%s" (Tid.name (Term.tid t)))

  let connect tid taint =
    Machine.Local.update mapper ~f:(fun m -> {
          m with
          tids = Map.add m.tids ~key:taint ~data:tid
        })


  let gentaint t =
    kind t >>= fun k ->
    Object.create k >>= fun taint ->
    connect (Term.tid t) taint >>| fun () ->
    Taint.Object.Set.singleton taint

  let introduces def =
    Term.has_attr def Legacy_taint.reg ||
    Term.has_attr def Legacy_taint.ptr

  let taint_ptr taints ptr sz =
    Eval.exp ptr >>= fun ptr ->
    Machine.Seq.iter (Seq.range 0 (Size.in_bytes sz)) ~f:(fun off ->
        Value.nsucc ptr off >>= fun ptr ->
        Tracker.attach ptr Taint.Rel.indirect taints)

  let taint_var taints var =
    Env.get var >>= fun v ->
    Tracker.attach v Taint.Rel.direct taints

  let intro def =
    if introduces def then
      gentaint def >>= fun t ->
      taint_var t (Def.lhs def) >>= fun () ->
      match Def.rhs def with
      | Bil.Load (_,addr,_,sz)
      | Bil.Store (_,addr,_,_,sz) ->
        taint_ptr t addr sz
      | exp ->
        Exp.free_vars exp |> Set.to_sequence |>
        Machine.Seq.iter ~f:(taint_var t)
    else Machine.return ()

  let init () =
    Primus.Interpreter.leave_def >>> intro
end



(* mapper maintains a state that maps variables (indexed by term
   identifier) to the set of taints (represented as tids) attached to
   those variables. Since the new framework represents taints as
   arbitrary values classified with their kind, we also need a mapping
   that will establish a correspondence between new and old
   representation, this mapping is maintained by the Intro component,
   that is responsible for introducing taints that were explicitly
   marked with the taint attribute.
*)
module Mapper(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Env = Primus.Env.Make(Machine)
  module Tracker = Taint.Tracker(Machine)

  let relations = [
    Taint.Rel.indirect, Fields_of_mapper.ptrs;
    Taint.Rel.direct, Fields_of_mapper.regs;
  ]

  let remap tids objs =
    Tid.Set.filter_map objs ~f:(Map.find tids)

  let update vars term =
    Machine.List.iter relations ~f:(fun (rel,fld) ->
        Machine.Local.get mapper >>= fun s ->
        Set.to_sequence (vars term) |>
        Machine.Seq.fold ~init:s ~f:(fun s var ->
            Env.get var >>= fun v ->
            Tracker.lookup v rel >>| remap s.tids >>| fun taints ->
            Field.fset fld s @@
            Map.update (Field.get fld s) (Term.tid term) ~f:(function
                | None -> Var.Map.singleton var taints
                | Some taints' ->
                  Map.update taints' var ~f:(function
                      | None -> taints
                      | Some taints' -> Set.union taints taints'))) >>=
        Machine.Local.put mapper)

  let update_jmp = update Jmp.free_vars
  let update_def = update Def.free_vars


  let init () = Machine.sequence Primus.Interpreter.[
      leave_def >>> update_def;
      leave_jmp >>> update_jmp;
    ]
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
      mark Legacy_taint.regs regs |>
      mark Legacy_taint.ptrs ptrs
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

let markers : Primus.component list = [
  (module Mapper);
  (module Marker);
]


open Config;;
manpage [
  `S "DESCRIPTION";
  `P "The Primus taint propagatation engine.";

  `P "Integrates the Primus Taint Analysis Framework with the old taint
    propagation framework. The new framework uses signals and
    observations, provides sanitization operations and tracks
    taint liveness, that enables more conventional and online
    taint analysis. The old taint propagation framework used a
    pipeline approach, with taints attributed to terms. It is still
    used by some existing analysis and tooling. This module reflects
    the information produced by the Primus Taint Analysis Framework
    to the old representation, i.e., taint attributes.";
]

let deprecated = "is deprecated, use the primus-taint plugin instead"


let enabled = flag "run" ~doc:deprecated
let don't_mark = flag "no-marks" ~doc:deprecated

(* deprecation doesn't work as expected with flags, so let's invent
 * something here... *)
let () = when_ready (fun {get=(!!)} ->
    if !!enabled
    then eprintf
        "Warning: this option is deprecated and propagation is now
         controlled by the primus-taint plugin (and there is no need
         to enable the propagation anymore)\n%!";
    if not !!don't_mark  (* sorry :)  *)
    then List.iter markers ~f:Primus.Machine.add_component)
