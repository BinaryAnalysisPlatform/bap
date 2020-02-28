open Core_kernel
open Bap.Std
open Monads.Std
open Bap_primus.Std
module Legacy_taint = Taint [@@warning "-D"]
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

let is_mem v = match Var.typ v with
  | Type.Mem _ -> true
  | _ -> false


module Intro(Machine : Primus.Machine.S) = struct
  module Env = Primus.Interpreter.Make(Machine)
  module Eval = Primus.Interpreter.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  module Tracker = Taint.Tracker.Make(Machine)
  module Kind = Taint.Kind.Make(Machine)
  module Object = Taint.Object.Make(Machine)

  open Machine.Syntax

  let kind t =
    Kind.create (sprintf "user-%s" (Tid.name t))

  let connect tid taint =
    Machine.Local.update mapper ~f:(fun m -> {
          m with
          tids = Map.set m.tids ~key:taint ~data:tid
        })

  let translation = [
    Legacy_taint.reg, `Reg, Taint.Rel.direct;
    Legacy_taint.ptr, `Ptr, Taint.Rel.indirect;
  ]


  let gentaint t =
    kind t >>= fun k ->
    Object.create k >>= fun taint ->
    connect t taint >>| fun () ->
    Taint.Object.Set.singleton taint

  let introduces def =
    List.find_map translation ~f:(fun (k,s,r) ->
        match Term.get_attr def k with
        | None -> None
        | Some t -> Some (t,s,r))

  let taint_ptr taints ptr =
    Tracker.attach ptr Taint.Rel.indirect taints

  let taint_var k taints var =
    if is_mem var then Machine.return ()
    else
      Env.get var >>= fun v ->
      Tracker.attach v k taints

  let intro def =
    match introduces def with
    | None -> Machine.return ()
    | Some (taint,kind,rel) ->
      gentaint taint >>= fun t ->
      taint_var rel t (Def.lhs def) >>= fun () ->
      match Def.rhs def with
      | Bil.Load (_,addr,ed,sz)
      | Bil.Store (_,addr,_,ed,sz) ->
        Eval.exp addr >>= fun ptr ->
        if [%compare.equal: [`Reg | `Ptr] ] kind `Reg
        then taint_ptr t ptr
        else
          Eval.load ptr ed sz >>=
          taint_ptr t
      | exp ->
        Exp.free_vars exp |> Set.to_sequence |>
        Machine.Seq.iter ~f:(taint_var rel t)

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
  module Tracker = Taint.Tracker.Make(Machine)

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
            if Set.is_empty taints || is_mem var then s
            else
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


  let update_taints t tag ts = match Term.get_attr t tag with
    | None -> Term.set_attr t tag ts
    | Some ts' -> Term.set_attr t tag @@ Map.merge ts ts'
        ~f:(fun ~key:_ -> function
            | `Left ts | `Right ts -> Some ts
            | `Both (ts,ts') -> Some(Set.union ts ts'))


  let mark tag taints t = match Map.find taints (Term.tid t) with
    | Some ts when not(Map.is_empty ts) -> update_taints t tag ts
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

let markers : (string * Primus.component) list = [
  "mapper",(module Mapper);
  "marker", (module Marker);
]

let enable_projection () =
  List.iter markers ~f:(fun (_,comp) ->
      Primus.Machine.add_component comp [@warning "-D"])

let enable_injection () =
  Primus.Machine.add_component (module Intro) [@warning "-D"]

open Config;;
manpage [
  `S "DESCRIPTION";

  `P "This plugin implements a compatibility layer between the new
    Primus Taint Analysis Framework and the old taint propagation
    framework (the $(b,propagate-taint) plugin). The new framework
    uses the pubslisher-subscriber pattern, provides sanitization
    operations, and tracks the taints liveness, that enables more
    conventional and online taint analysis. However it represents
    taints as abstract objects associated with computations (values),
    while the old taint propagation framework uses a pipeline
    approach, with taints represented as attributes attached to
    program terms. Since the new representation of taints is much more
    precise and there is no bijection between terms and values, this
    layer will loose information due to this impendance mismatch. The
    trade-offs of the translation and described below. New analysis,
    if possible, shall rely on the new framework.";

  `P "The translation is achieved by mapping the $(b,tainted-ptr) and
    $(b,tainted-reg) attributes to corresponding taint introduction
    operations of the Primus Taint Analysis Framework, and by
    reflecting the taint state of the analysis into the
    $(b,tainted-regs) and $(b,tainted-ptrs) attributes. Both steps are
    optional, and could be enabled and disabled individually.";

  `P
    "Since an attribute is attached to the whole term not to an
    individual expression or value we need some rule that prescribes
    how terms maps to values. If a term is marked as a term that
    introduces a taint, then we assume that a value, computed
    in this term, references the tainted object either directly (in
    case of $(b,tainted-reg)) or indirectly (in case of
    $(b,tainted-ptr)). We always taint a value contained in the
     left-hand side of a definition. In addition, we also try to
    taint values on the right hand side. If there is a load or store
    operation, then we taint address as a pointer to the object that
    will track, if it was marked with the $(b,tainted-reg)
    attribute. If it was marked with the $(b,tainted-ptr) attribute
    then we dereference this pointer and taint the dereferenced
    address. If the right hand side is an arbitrary expression, then
    we assume that all variables that are used in this expression
    contain values that are referencing directly or indirectly the
    tainted object."
]

let injection_enabled = flag "from-attributes"
    ~doc:"Introduces taint in terms that are marked with the
    $(b,tainted-ptr) and $(b,tainted-reg) attribute. "

let projection_enabled = flag "to-attributes"
    ~doc:"Reflects the state of the taint propagation engine to the
    $(b,tainted-ptrs) and $(b,tainted-regs) term attributes."

let soft_deprecation_notice = {|
This option is left for compatibility with the old interface and
is not compatible with the $(b,from-attributes) or
$(b,to-attrbutes) options. It is an error to mix options from the
new and old interfaces.
|}

let enabled = flag "run"
    ~doc:("Enables propagating taint from term attributes and back to \
           attributes, unless the latter is disabled with the
           $(b,no-marks) option. " ^ soft_deprecation_notice)

let marking_disabled = flag "no-marks"
    ~doc: ("Disables the projection of the taint engine state to term
    attributes. The option is only valid when the $(b,run) option is
    specified. " ^ soft_deprecation_notice)

;;

when_ready begin fun {get=is} ->
  if is injection_enabled || is projection_enabled
  then if is enabled || is marking_disabled
    then invalid_arg "Incorrect mix of old and new parameters";
  if is marking_disabled && not (is enabled)
  then invalid_arg "The no-marks option is only valid if \
                    the run option is specified";

  List.iter markers ~f:(fun (name,comp) ->
      Primus.Components.register_generic name comp
        ~package:"primus-propagate-taint");

  Primus.Components.register_generic "intro" (module Intro)
    ~package:"primus-propagate-taint";

  (* Modern interface *)
  if is injection_enabled
  then enable_injection ();
  if is projection_enabled
  then enable_projection ();

  (* Legacy interface *)
  if is enabled then begin
    enable_injection ();
    if not (is marking_disabled) then
      enable_projection ();
  end
end
