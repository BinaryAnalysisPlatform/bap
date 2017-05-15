open Core_kernel.Std
open Bap.Std
open Monads.Std
open Bap_primus.Std
include Self()
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
   for every block B that is terminated with m jumps, will fork m
   context after a last definition, so that under the n-th context
   the Dn destination will be taken by the interpreter.

   For the Dm-th destination to be taken, the following condition must
   hold: {v ~C1 /\ ~C2 /\ ... ~C(n-1) /\ Cn v}, where [~] symbol
   denotes logical negation.

   However, we would require an SMT solver to find such contexts. So,
   the intepreter requires a program to be in a trivial condition form
   (TCF). In TCF every jmp condition must be a single variable or a
   constant true.


*)
type assn = {
  use : tid;
  var : var;
  res : bool;
}

type id = Primus.Machine.id
type conflict = Conflict of assn list


let inspect_assn {use; var; res} =
  let var = sprintf "%s: %s" (Tid.to_string use)(Var.to_string var) in
  Sexp.List [Sexp.Atom var; sexp_of_bool res]

let inspect_conflict (Conflict assn) = Sexp.List (List.map assn ~f:inspect_assn)
let inspect_conflicts cs =
  Sexp.List (List.map ~f:inspect_conflict cs)

let state = Primus.Machine.State.declare
              ~inspect:inspect_conflict
              ~name:"conflicts"
              ~uuid:"58bb35f4-f259-4712-8d15-bdde1be3caa8"
              (fun _ -> Conflict [])

let blk_without_jmps = Term.filter jmp_t ~f:(fun _ -> false)
let new_var () = Var.create ~is_virtual:true ~fresh:true "c" bool_t

(* Pre: number of jumps is greater than 1
   post: number of jumps is the same, each jump is in TCF.*)
let blk blk =
  Term.enum jmp_t blk |>
  Seq.fold ~init:(blk_without_jmps blk) ~f:(fun blk jmp ->
      match Jmp.cond jmp with
      | Bil.Int _ | Bil.Var _ -> Term.append jmp_t blk jmp
      | cond ->
        let var = new_var () in
        let def = Def.create var cond in
        let blk = Term.append def_t blk def in
        let jmp = Jmp.with_cond jmp (Bil.var var) in
        Term.append jmp_t blk jmp)

let sub = Term.map blk_t ~f:(fun b ->
    if Term.length jmp_t b < 2 then b else blk b)

let prog = Term.map sub_t ~f:sub

let neg = List.map ~f:(fun assn -> {assn with res = not assn.res})

let assumptions blk =
  Term.enum jmp_t blk |> Seq.fold ~init:([],[])
    ~f:(fun (assns, assms) jmp -> match Jmp.cond jmp with
        | Bil.Var c ->
          let assn = {use=Term.tid jmp; var=c; res=true} in
          assn :: assns, (assn :: neg assns) :: assms
        | Bil.Int _ -> assns, (neg assns) :: assms
        | _ -> (assns,assms)) |> snd

module Main(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Interpreter = Primus.Interpreter.Make(Machine)

  let assume assns =
    let self = new Interpreter.t in
    Machine.List.iter assns ~f:(fun assn ->
        self#eval_int (Word.of_bool assn.res) >>=
        self#update assn.var)

  let unsat_assumptions blk =
    let self = new Interpreter.t in
    Machine.List.map (assumptions blk)
      ~f:(Machine.List.filter ~f:(fun assn ->
          let exp = Word.of_bool assn.res in
          self#lookup assn.var >>| Bil.Result.value >>| function
          | Bil.Imm r -> Word.(r <> exp)
          | _ -> true))

  let fork blk  =
    unsat_assumptions blk >>=
    Machine.List.iter ~f:(fun assns ->
        Machine.fork () >>= fun () ->
        assume  assns >>= fun () ->
        Machine.current () >>= fun id ->
        Machine.Local.put state (Conflict assns) >>= fun () ->
        Machine.parent () >>= fun pid ->
        Machine.switch pid)

  let is_last blk def = Term.last def_t blk = Some def

  let step level =
    let open Primus.Context.Level in
    match level with
    | Def {up={me=blk}; me=def} when is_last blk def ->
      fork blk
    | _ -> Machine.return ()

  let init () =
    Primus.Interpreter.leave_level >>> step
end

open Config;;

manpage [
  `S "DESCRIPTION";
  `P
    "When this mode is enabled the Primus Machine will venture into
     paths with unsatisfied constraints. Basically, it means that on
     every branch the state is duplicated.  "
]

let enabled = flag "mode" ~doc:"Enable the mode."


let () = when_ready (fun {get=(!!)} ->
    if !!enabled then
      Primus.Machine.add_component (module Main))
