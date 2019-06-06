open Core_kernel
open Bap_types.Std
open Graphlib.Std
open Bap_image_std
open Bap_disasm_std
open Bap_ir
open Format
module IRLabel = Label
open Bap_core_theory
open Bap_knowledge
module L = Label
module Label = IRLabel


let update_jmp jmp ~f =
  f (Ir_jmp.dst jmp) (Ir_jmp.alt jmp) @@ fun ~dst ~alt ->
  Ir_jmp.reify
    ~tid:(Term.tid jmp)
    ?cnd:(Ir_jmp.guard jmp)
    ?dst ?alt ()

let intra_fall cfg block =
  Seq.find_map (Cfg.Node.outputs block cfg) ~f:(fun e ->
      match Cfg.Edge.label e with
      | `Fall -> Option.some @@
        Ir_jmp.resolved (Tid.for_addr (Block.addr (Cfg.Edge.dst e)))
      | _ -> None)

(* a subroutine could be called implicitly via a fallthrough,
   therefore it will not be reified into jmp terms automatically,
   so we need to do some work here.
   We look into the symtable for an implicit call, and if
   such exists and it is physically present in the symtab, then we
   return a tid for the address, otherwise we return a tid for the
   subroutine name. It is important to return the tid for address,
   so that we can compare tids in the [insert_call] function. *)
let inter_fall symtab block =
  let open Option.Monad_infix in
  symtab >>= fun symtab ->
  Symtab.implicit_callee symtab (Block.addr block) >>| fun name ->
  Ir_jmp.resolved @@
  match Symtab.find_by_name symtab name with
  | None -> Tid.for_name name
  | Some (_,entry,_) -> Tid.for_addr (Block.addr entry)

module IrBuilder = struct

  let def_only blk = Term.length jmp_t blk = 0

  (* concat two def-only blocks *)
  let append_def_only b1 b2 =
    let b = Ir_blk.Builder.init ~same_tid:true ~copy_defs:true b1 in
    Term.enum def_t b2 |> Seq.iter ~f:(Ir_blk.Builder.add_def b);
    Term.enum jmp_t b2 |> Seq.iter ~f:(Ir_blk.Builder.add_jmp b);
    Ir_blk.Builder.result b

  let append xs ys = match xs, ys with
    | [],xs | xs,[] -> xs
    | x :: xs, y :: ys when def_only x ->
      List.rev_append ys (append_def_only x y :: xs)
    | xs, ys -> List.rev_append ys xs

  let ir_of_insn insn =
    Toplevel.eval Term.slot begin
      let open Knowledge.Syntax in
      let cls = Theory.Program.Semantics.cls in
      Knowledge.Object.create cls >>= fun obj ->
      Knowledge.provide Bil.slot obj (Insn.bil insn) >>| fun () ->
      obj
    end

  let set_attributes ?mem insn blks =
    let addr = Option.map ~f:Memory.min_addr mem in
    let set_attributes k b =
      Term.map k b ~f:(fun t ->
          let t = Term.set_attr t Disasm.insn insn in
          Option.value_map addr ~f:(Term.set_attr t address)
            ~default:t) in
    List.map blks ~f:(fun blk ->
        set_attributes jmp_t blk |>
        set_attributes def_t)

  let lift_insn ?mem insn blks =
    append blks @@
    set_attributes ?mem insn (ir_of_insn insn)

  let with_first_blk_addressed addr = function
    | [] -> []
    | b :: bs -> Term.set_attr b address addr :: bs

  let turn_into_call ret blk =
    Term.map jmp_t blk ~f:(update_jmp ~f:(fun dst _ jmp ->
        jmp ~dst:ret ~alt:dst))

  let landing_pad return jmp =
    match Ir_jmp.kind jmp with
    | Int (_,pad) ->
      let pad = Ir_blk.create ~tid:pad () in
      let pad = match return with
        | None -> pad
        | Some dst -> Term.append jmp_t pad (Ir_jmp.reify ~dst ()) in
      Some pad
    | _ -> None

  let with_landing_pads return bs = match bs with
    | [] -> []
    | b :: bs as blks ->
      let pads = List.fold ~init:[] blks ~f:(fun pads b ->
          Term.enum jmp_t b |>
          Seq.fold ~init:pads ~f:(fun pads jmp ->
              match landing_pad return jmp with
              | Some pad -> pad :: pads
              | None -> pads)) in
      b :: List.rev_append pads bs

  let resolves_equal x y =
    match Ir_jmp.resolve x, Ir_jmp.resolve y with
    | First x, First y -> Tid.equal x y
    | _ -> false

  let insert_call alt blk =
    let same_tid = resolves_equal alt in
    let alt = Some alt in
    match Term.last jmp_t blk with
    | None -> [Term.append jmp_t blk @@ Ir_jmp.reify ?alt ()]
    | Some jmp -> update_jmp jmp ~f:(fun dst alt' jmp ->
        match dst,alt' with
        | dst,None -> [Term.update jmp_t blk @@ jmp ~dst ~alt]
        | _,Some alt' when same_tid alt' -> [blk]
        | _,Some alt' ->
          let call =
            Term.append jmp_t (Ir_blk.create ()) @@
            Ir_jmp.reify ?alt () in
          let dst = Some (Ir_jmp.resolved (Term.tid call)) in
          [
            call;
            Term.update jmp_t blk @@ jmp ~dst ~alt:(Some alt');
          ])

  let blk ?symtab cfg block : blk term list =
    let tid = Tid.for_addr (Block.addr block) in
    let blks =
      Block.insns block |>
      List.fold ~init:[Ir_blk.create ~tid ()] ~f:(fun blks (mem,insn) ->
          lift_insn ~mem insn blks) in
    let fall = intra_fall cfg block in
    let blks = with_landing_pads fall blks in
    with_first_blk_addressed (Block.addr block) @@
    match blks,fall,Insn.(is call) (Block.terminator block) with
    | [],_,_ -> []
    | x::xs, Some _, true ->
      List.rev (turn_into_call fall x :: xs)
    | x::xs, Some dst, false ->
      List.rev (Term.append jmp_t x (Ir_jmp.reify ~dst ()) :: xs)
    | x::xs, None, is_call ->
      let x = if is_call then turn_into_call fall x else x in
      match inter_fall symtab block with
      | None -> List.rev (x::xs)
      | Some dst -> List.rev (insert_call dst x @ xs)
end

let blk cfg block = IrBuilder.blk cfg block


let lift_sub ?tid entry cfg =
  let addrs = Addr.Table.create () in
  let recons acc b =
    let addr = Block.addr b in
    let blks = blk cfg b in
    Option.iter (List.hd blks) ~f:(fun blk ->
        Hashtbl.add_exn addrs ~key:addr ~data:(Term.tid blk));
    acc @ blks in
  let blocks = Graphlib.reverse_postorder_traverse
      (module Cfg) ~start:entry cfg in
  let blks = Seq.fold blocks ~init:[] ~f:recons in
  let n = let n = List.length blks in Option.some_if (n > 0) n in
  let sub = Ir_sub.Builder.create ?tid ?blks:n () in
  List.iter blks ~f:(Ir_sub.Builder.add_blk sub);
  let sub = Ir_sub.Builder.result sub in
  Term.set_attr sub address (Block.addr entry)


(* Rewires some intraprocedural jmps into interprocedural.

   - If a jmp is unresolved, we can't really prove that the control
     flow won't leave the boundaries of its subroutine, therefore we
     conservatively turn it into a call.

   - If a jmp is resolved, but its destination is not within
     the boundaries of its function, then we reclassify it as
     interprocedural.

   - If a jmp already has an alternate route, to keep things simple,
     we do not touch it. *)
let alternate_nonlocal sub jmp =
  let needs_alternation =
    Option.is_none (Ir_jmp.alt jmp) && match Ir_jmp.dst jmp with
    | None -> false  (* already nonlocal *)
    | Some dst -> match Ir_jmp.resolve dst with
      | Second _ -> true  (* all unresolved are potentially calls *)
      | First dst -> Option.is_none (Term.find blk_t sub dst) in
  if needs_alternation
  then update_jmp jmp ~f:(fun dst _ jmp -> jmp ~dst:None ~alt:dst)
  else jmp

(* On the local level of lifting all resolved jmps are pointing to
   basic blocks. When we lift on the program (global) level, we
   need to link all resolved calls to subroutines.

   The [sub_of_blk] mapping maps tids of entries (basic blocks) to
   tids of their corresponding subroutines. If jmp intraprocedural
   edge points to an entry of a subroutine we relink it to that
   subroutine.

   If a jump is an interprocedural edge, that points to an entry of
   a subroutine we turn it into a tail call. This could be a real tail
   call, a self recursive call, or an error from the previous steps of
   lifting. In any case, it will guarantee that the entry block has
   indegree zero.

   Finally, if both jump edges are not pointing to the entries and
   the interprocedural edge is resolved, then we have a jump to an
   external function, so we relink it to a label that corresponds
   to the name of the external function. Note, this step relies on
   the [alternate_nonlocal] pass, described above. *)
let link_call symtab addr sub_of_blk jmp =
  let open Option.Monad_infix in
  let resolve dst = dst jmp >>| Ir_jmp.resolve >>= function
    | Second _ -> None
    | First tid -> Some tid in
  let sub_of_dst dst =
    resolve dst >>= Hashtbl.find sub_of_blk >>| Ir_jmp.resolved in
  let external_callee () =
    addr >>= Symtab.explicit_callee symtab >>| Tid.for_name >>|
    Ir_jmp.resolved in
  match sub_of_dst Ir_jmp.dst, sub_of_dst Ir_jmp.alt with
  | _, (Some _ as alt) ->
    update_jmp jmp ~f:(fun dst _ jmp -> jmp ~dst ~alt)
  | Some _ as alt, None ->
    update_jmp jmp ~f:(fun _ _ jmp -> jmp ~dst:None ~alt)
  | None,None -> match resolve Ir_jmp.alt with
    | None -> jmp
    | Some (_:tid) -> match external_callee () with
      | Some alt -> update_jmp jmp ~f:(fun dst _ jmp ->
          jmp ~dst ~alt:(Some alt))
      | None -> jmp

let program symtab =
  let b = Ir_program.Builder.create () in
  let sub_of_blk = Hashtbl.create (module Tid) in
  Seq.iter (Symtab.to_sequence symtab) ~f:(fun (name,entry,cfg) ->
      let addr = Block.addr entry in
      let blk_tid = Tid.for_addr addr in
      let sub_tid = Tid.for_name name in
      let sub = lift_sub ~tid:sub_tid entry cfg in
      Ir_program.Builder.add_sub b (Ir_sub.with_name sub name);
      Hashtbl.add_exn sub_of_blk ~key:blk_tid ~data:sub_tid;);
  let program = Ir_program.Builder.result b in
  Term.map sub_t program ~f:(fun sub ->
      Term.map blk_t sub ~f:(fun blk ->
          let addr = Term.get_attr blk address in
          Term.map jmp_t blk ~f:(fun jmp ->
              jmp |>
              alternate_nonlocal sub |>
              link_call symtab addr sub_of_blk)))

let sub blk cfg = lift_sub blk cfg

let insn insn =
  List.rev @@ IrBuilder.lift_insn insn [Ir_blk.create ()]
