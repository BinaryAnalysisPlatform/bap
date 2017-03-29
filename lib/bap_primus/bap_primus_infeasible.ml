open Core_kernel.Std
open Bap.Std
open Monads.Std

open Bap_primus_types

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
module Infeasible = struct

  type assn = {
    use : tid;
    var : var;
    res : bool;
  }

  type 'a conflict = Conflict of assn list * 'a

  module type S2 = sig
    type ('a,'e) t

    type id

    val fork : blk term -> (id conflict list,'e)  t
  end

  (* pre: number of jumps is greater than 1
     post: number of jumps is the same, each jump is in TCF.*)
  let blk blk =
    Term.enum jmp_t blk |>
    Seq.fold ~init:(Term.filter jmp_t blk ~f:(fun _ -> true))
      ~f:(fun blk jmp -> match Jmp.cond jmp with
          | Bil.Int _ | Bil.Var _ -> Term.append jmp_t blk jmp
          | cond ->
            let var =
              Var.create ~is_virtual:true ~fresh:true "c" bool_t in
            let def = Def.create var cond in
            let blk = Term.append def_t blk def in
            let jmp = Jmp.with_cond jmp (Bil.var var) in
            Term.append jmp_t blk jmp)

  let sub = Term.map blk_t ~f:(fun b ->
      if Term.length jmp_t b = 1 then b else blk b)

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



  module Make(SM : Machine) = struct
    open SM.Syntax
    type id = SM.id
    module Expi = Expi.Make(SM)

    let apply self  =
      SM.List.iter ~f:(fun assn ->
          self#eval_int (Word.of_bool assn.res) >>=
          self#update assn.var)

    let fork (self : 'e #Expi.t) blk : (id conflict list,'e) SM.t =
      SM.List.map (assumptions blk)
        ~f:(SM.List.filter ~f:(fun assn ->
              let exp = Word.of_bool assn.res in
              self#lookup assn.var >>| Bil.Result.value >>| function
              | Bil.Imm r -> Word.(r <> exp)
              | _ -> true)) >>=
      SM.List.map ~f:(fun assns ->
          SM.fork () >>= fun () ->
          apply self assns >>= fun () ->
          SM.current () >>= fun id ->
          SM.parent () >>= fun pid ->
          SM.switch pid >>| fun () ->
          Conflict (assns,id))
  end

end
