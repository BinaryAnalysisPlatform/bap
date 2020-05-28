open Core_kernel
open Bap.Std
open Monads.Std
open Bap_primus.Std
open Format
include Self()

let package = "bap"

(*

   In a general case a block is terminated by a sequence of jumps:

   {v
     when C1 jmp D1
     when C2 jmp D2
     ...
     when Cm jmp Dm
   v}

   The IR requires the following invariant:

   {v C1 \/ C2 \/ ... \/ Cm v}.

   The infeasible interpreter is a non-deterministic interperter, that
   for every block B that is terminated with m jumps, will fork m-1
   context after the last definition, so that under the n-th context
   the Dn destination will be taken by the interpreter.

   For the Dm-th destination to be taken, the following condition must
   hold: {v ~C1 /\ ~C2 /\ ... ~C(n-1) /\ Cn v}, where [~] symbol
   denotes logical negation.

   However, we would require an SMT solver to find such contexts. So,
   the interpreter requires a program to be in a trivial condition form
   (TCF). In TCF every jmp condition must be a single variable or a
   constant true.
*)

type assn = {
  use : tid;
  var : var;
  res : bool;
}

type id = Primus.Machine.id
type state = {
  conflicts : assn list;
  visited : Tid.Set.t;
}

let inspect_assn {use; var; res} =
  let var = sprintf "%s: %s" (Tid.to_string use)(Var.to_string var) in
  Sexp.List [Sexp.Atom var; sexp_of_bool res]

let inspect_conflict (assn) =
  Sexp.List (List.map assn ~f:inspect_assn)

let inspect_conflicts cs =
  Sexp.List (List.map ~f:inspect_conflict cs)

let init_visited prog =
  (object inherit [Tid.Set.t] Term.visitor
    method! enter_blk blk visited =
      if Term.has_attr blk Term.visited
      then Set.add visited (Term.tid blk)
      else visited
  end)#run prog Tid.Set.empty

let state = Primus.Machine.State.declare
    ~name:"conflicts"
    ~uuid:"58bb35f4-f259-4712-8d15-bdde1be3caa8" @@
  fun proj ->
  {
    conflicts=[];
    visited = init_visited (Project.program proj)
  }

let neg = List.map ~f:(fun assn -> {assn with res = not assn.res})

let assumptions blk =
  Term.enum jmp_t blk |> Seq.fold ~init:([],[])
    ~f:(fun (assns, assms) jmp -> match Jmp.cond jmp with
        | Bil.Var c ->
          let assn = {use=Term.tid jmp; var=c; res=true} in
          assn :: assns, (assn :: neg assns) :: assms
        | Bil.Int _ -> assns, (neg assns) :: assms
        | _ -> failwith "Not in TCF") |> snd


module TrapPageFault(Machine : Primus.Machine.S) = struct
  module Code = Primus.Linker.Make(Machine)
  let exec =
    Code.unlink (`symbol Primus.Interpreter.pagefault_handler)
end

module DoNothing(Machine : Primus.Machine.S) = struct
  let exec = Machine.return ()
end

module Id = Monad.State.Multi.Id

module Forker(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Eval = Primus.Interpreter.Make(Machine)
  module Env = Primus.Env.Make(Machine)
  module Mem = Primus.Memory.Make(Machine)
  module Linker = Primus.Linker.Make(Machine)

  let assume assns =
    Machine.List.iter assns ~f:(fun assn ->
        Eval.const (Word.of_bool assn.res) >>= fun r ->
        Eval.get assn.var >>= fun r' ->
        let op = if assn.res then Bil.OR else Bil.AND in
        Eval.binop op r r' >>=
        Eval.set assn.var)

  let unsat_assumptions blk =
    Machine.List.map (assumptions blk)
      ~f:(Machine.List.filter ~f:(fun {var;res} ->
          Env.get var >>| Primus.Value.to_word >>| fun r ->
          Word.(r <> of_bool res)))

  let pp_id = Monad.State.Multi.Id.pp

  let do_fork blk ~child =
    Machine.current () >>= fun pid ->
    Machine.Global.get state >>= fun t ->
    if Set.mem t.visited (Term.tid blk)
    then Machine.return pid
    else
      Machine.fork () >>= fun () ->
      Machine.current () >>= fun id ->
      if Id.(pid = id) then Machine.return id
      else
        child () >>= fun () ->
        Machine.switch pid >>| fun () -> id


  let fork blk  =
    unsat_assumptions blk >>=
    Machine.List.iter ~f:(function
        | [] -> Machine.return ()
        | conflicts ->
          Machine.ignore_m @@
          do_fork blk ~child:(fun () ->
              assume  conflicts >>= fun () ->
              Machine.Local.update state ~f:(fun t -> {
                    t with conflicts})))

  let assume_returns blk call =
    match Call.return call with
    | Some (Direct dst) ->
      Machine.current () >>= fun pid ->
      do_fork blk ~child:Machine.return >>= fun id ->
      if Id.(id = pid) then Machine.return ()
      else
        Machine.Global.get state >>= fun {visited} ->
        if Set.mem visited dst
        then Eval.halt >>= never_returns
        else
          Linker.exec (`tid dst) >>= fun () ->
          Eval.halt >>= never_returns
    | _ -> Machine.return ()

  let fork_on_calls blk jmp = match Jmp.kind jmp with
    | Call c -> assume_returns blk c
    | _ -> Machine.return ()

  let is_last blk def = match Term.last def_t blk with
    | None -> true
    | Some last -> Term.same def last

  let has_no_def blk = Term.length def_t blk = 0

  let step level =
    let open Primus.Pos in
    match level with
    | Blk {me=blk} when has_no_def blk -> fork blk
    | Def {up={me=blk}; me=def} when is_last blk def ->
      fork blk
    | Jmp {up={me=blk}; me=jmp} -> fork_on_calls blk jmp
    | _ -> Machine.return ()

  let mark_visited blk =
    Machine.Global.update state ~f:(fun t -> {
          t with visited =
                   Set.add t.visited (Term.tid blk)
        })


  let init () = Machine.sequence [
      Primus.Interpreter.leave_pos >>> step;
      Primus.Interpreter.leave_blk >>> mark_visited;
    ]
end

module EnableDivisionByZero(Machine : Primus.Machine.S) = struct
  module Linker = Primus.Linker.Make(Machine)
  let init () =
    Linker.link ~name:Primus.Interpreter.division_by_zero_handler
      (module DoNothing)
end

let legacy_promiscous_mode_components = [
  "var-randomizer";
  "mem-randomizer";
  "arg-randomizer";
  "promiscuous-path-explorer";
  "division-by-zero-handler";
  "limit";
]

let enable_legacy_promiscuous_mode () =
  Primus.System.Repository.update ~package "legacy-main" ~f:(fun init ->
      List.fold legacy_promiscous_mode_components ~init
        ~f:(fun system component ->
            Primus.System.add_component system ~package component))

open Config;;

let desc =
  "When this mode is enabled the Primus Machine will venture into \
   paths with unsatisfied constraints. Basically, it means that on \
   every branch the state is duplicated."
;;

manpage [
  `S "DESCRIPTION";
  `P desc ;
  `P
    "The program will be translated into the Trivial Condition Form,
  where each compound condition expression is trivialized to a
  variable, that is bound earlier in the block."

]

let enabled = flag "mode" ~doc:"(DEPRECATED) Enable the mode."

let () = when_ready (fun {get=(!!)} ->
    Primus.Components.register_generic "promiscuous-path-explorer"
      (module Forker) ~package
      ~desc:"Forces execution of all linearly independent paths \
             by forcefully flipping the branch conditions.";
    Primus.Components.register_generic "division-by-zero-handler"
      (module EnableDivisionByZero) ~package
      ~desc:"Disables division by zero errors.";

    if !!enabled then enable_legacy_promiscuous_mode ());
