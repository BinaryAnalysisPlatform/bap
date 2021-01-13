open Bap_core_theory

open Core_kernel
open Bap_types.Std
open Graphlib.Std
open Bap_image_std
open Bap_disasm_std
open Bap_ir

let update_jmp jmp ~f =
  f (Ir_jmp.dst jmp) (Ir_jmp.alt jmp) @@ fun ~dst ~alt ->
  let jmp' = Ir_jmp.reify
      ~tid:(Term.tid jmp)
      ?cnd:(Ir_jmp.guard jmp)
      ?dst ?alt () in
  Term.with_attrs jmp' (Term.attrs jmp)

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

let has_explicit_call symtab block =
  match symtab with
  | None -> false
  | Some symtab ->
    Option.is_some (Symtab.explicit_callee symtab (Block.addr block))

let has_inter_edges symtab block =
  match symtab with
  | None -> false
  | Some symtab ->
    let src = Block.addr block in
    Option.is_some (Symtab.explicit_callee symtab src) ||
    Option.is_some (Symtab.implicit_callee symtab src)

module IrBuilder = struct

  let def_only blk = Term.length jmp_t blk = 0

  (* concat two def-only blocks *)
  let append_def_only b1 b2 =
    let b = Ir_blk.Builder.init ~same_tid:true ~copy_defs:true b1 in
    Term.enum def_t b2 |> Seq.iter ~f:(Ir_blk.Builder.add_def b);
    Term.enum jmp_t b2 |> Seq.iter ~f:(Ir_blk.Builder.add_jmp b);
    Ir_blk.Builder.result b

  (* [append xs ys]
     pre: the first block of [xs] is the exit block;
     pre: the first block of [ys] is the entry block;
     pre: the last block of [ys] is the exit block;
     post: the first block of [append xs ys] is the new exit block. *)
  let append xs ys = match xs, ys with
    | [],xs | xs,[] -> xs
    | x :: xs, y :: ys when def_only x ->
      List.rev_append ys (append_def_only x y :: xs)
    | x::xs, y::_ ->
      let jmp = Ir_jmp.reify ~dst:(Ir_jmp.resolved @@ Term.tid y) () in
      let x = Term.append jmp_t x jmp in
      List.rev_append ys (x::xs)

  let ir_of_insn insn = KB.Value.get Term.slot insn

  let set_attributes ?addr insn blks =
    let set_attributes k b =
      Term.map k b ~f:(fun t ->
          let t = Term.set_attr t Disasm.insn insn in
          Option.value_map addr ~f:(Term.set_attr t address)
            ~default:t) in
    List.map blks ~f:(fun blk ->
        set_attributes jmp_t blk |>
        set_attributes def_t)

  (* [lift_insn ?mem insn blks]
     pre: the first block of [blks] is the exit block;
     post: the first block of [lift_insn insn blks] is the new exit block. *)
  let lift_insn ?addr insn blks =
    append blks @@
    set_attributes ?addr insn (ir_of_insn insn)

  let with_first_blk_addressed addr = function
    | [] -> []
    | b :: bs -> Term.set_attr b address addr :: bs

  let is_unconditional jmp = match Ir_jmp.cond jmp with
    | Bil.Int _ ->  true
    | _ -> false

  let turn_into_call ret blk =
    Term.map jmp_t blk ~f:(fun jmp ->
        update_jmp jmp ~f:(fun dst alt make ->
            make
              ~dst:(if is_unconditional jmp then ret else None)
              ~alt:(Option.first_some alt dst)))

  let landing_pad return jmp =
    match Ir_jmp.kind jmp with
    | Int (_,pad) ->
      let pad = Ir_blk.create ~tid:pad () in
      let pad = match return with
        | Some (`Intra dst) -> Term.append jmp_t pad (Ir_jmp.reify ~dst ())
        | _ -> pad in
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

  let insert_inter_fall alt blk =
    Term.append jmp_t blk @@ Ir_jmp.reify ~alt ()

  let is_last_jump_unconditional blk =
    match Term.last jmp_t blk with
    | None -> false
    | Some jmp -> is_unconditional jmp

  let has_call blk =
    Term.enum jmp_t blk |>
    Seq.exists ~f:(fun jmp ->
        Option.is_some (Ir_jmp.alt jmp))

  let fall_if_possible dst blk =
    if is_last_jump_unconditional blk
    then blk
    else
      Term.append jmp_t blk @@
      Ir_jmp.reify ~dst ()

  let concat_map_fst_and_rev xs f =
    List.rev @@ match xs with
    | [] -> []
    | x :: xs -> f x @ xs

  let with_first_blk_optionally_addressed = function
    | None -> fun x -> x
    | Some addr -> with_first_blk_addressed addr

  (* packs a list of IR blks that represent blks obtained from
   * the instructions of the same basic block into a set of maximal
   * blocks, coalescing them whenever possible.
   *
   * Parameters:
   * - [is_call] designates if the last instruction in the basic block
   * is the call instruction. If set, then the last jump will be turned
   * into a call.
   * - [is_barrier] designates if the last instruction is a barrier,
   * i.e., such an instruction after which the control-flow never falls
   * to the next instruction or is no expected to be returned from a
   * subroutine.
   * - [fall] could be set to [`Intra dst] or [`Inter dst] and
   * designates the basic block fall through destination, which could
   * be either an intraprocedural or interprocedural control flow. If
   * the fall is interprocedural then it will be reified into a jump
   * of the call kind.
   *
   *
  *)
  let pack
    : ?is_call:bool ->
      ?is_barrier:bool ->
      ?fall:[> `Inter of Ir_jmp.dst | `Intra of Ir_jmp.dst ] ->
      ?addr:addr ->
      blk term list -> blk term list =
    fun ?(is_call=false) ?(is_barrier=false) ?fall ?addr blks ->
    let blks = with_landing_pads fall blks in
    with_first_blk_optionally_addressed addr @@
    concat_map_fst_and_rev blks @@ function
    | x when is_barrier -> [x]
    | x -> match fall with
      | None -> [if is_call || has_call x then turn_into_call None x else x]
      | Some (`Intra dst) -> [
          fall_if_possible dst @@
          if is_call || has_call x
          then turn_into_call (Some dst) x
          else x
        ]
      | Some (`Inter dst) ->
        if is_call || has_call x then
          let next = Ir_blk.create () in
          let fall = Ir_jmp.resolved (Term.tid next) in
          let next = insert_inter_fall dst next in [
            next;
            fall_if_possible fall @@
            turn_into_call (Some fall) x
          ]
        else [insert_inter_fall dst x]

  let insns ?fall ?addr insns =
    let tid = match addr with
      | None -> Tid.create ()
      | Some addr -> Tid.for_addr addr in
    let blks,termi = List.fold insns
        ~init:([Ir_blk.create ~tid ()],None)
        ~f:(fun (blks,_) insn ->
            lift_insn insn blks,Some insn) in
    match termi with
    | None -> blks
    | Some x ->
      pack blks
        ?fall
        ?addr
        ~is_call:(Insn.(is call x))
        ~is_barrier:Insn.(is barrier x)

  let blk ?symtab cfg block : blk term list =
    let addr = Block.addr block in
    let tid = Tid.for_addr addr in
    let blks =
      Block.insns block |>
      List.fold ~init:[Ir_blk.create ~tid ()] ~f:(fun blks (mem,insn) ->
          let addr = Memory.min_addr mem in
          lift_insn ~addr insn blks) in
    let fall = match intra_fall cfg block with
      | Some dst -> Some (`Intra dst)
      | None -> match inter_fall symtab block with
        | Some dst -> Some (`Inter dst)
        | None -> None in
    let x = Block.terminator block in
    pack blks
      ?fall
      ~addr
      ~is_call:(Insn.(is call x) || has_explicit_call symtab block)
      ~is_barrier:Insn.(is barrier x)
end

let blk cfg block = IrBuilder.blk cfg block

let lift_sub ?symtab ?tid entry cfg =
  let sub = Ir_sub.Builder.create ?tid ~blks:32 () in
  Graphlib.reverse_postorder_traverse (module Cfg) ~start:entry cfg |>
  Seq.iter ~f:(fun block ->
      let blks = IrBuilder.blk ?symtab cfg block in
      List.iter blks ~f:(Ir_sub.Builder.add_blk sub));
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
   tids of their corresponding subroutines. If an intraprocedural
   edge points to an entry of a subroutine we relink it to that
   subroutine.

   If a jump is an interprocedural edge, that points to an entry of
   a subroutine we turn it into a tail call. This could be a real tail
   call, a self recursive call, or an error from the previous steps of
   lifting. In any case, it will guarantee that the entry block has
   the local indegree zero.

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


let insert_synthetic prog =
  Term.enum sub_t prog |>
  Seq.fold ~init:prog ~f:(fun prog sub ->
      Term.enum blk_t sub |>
      Seq.fold ~init:prog ~f:(fun prog blk ->
          Term.enum jmp_t blk |>
          Seq.fold ~init:prog ~f:(fun prog jmp ->
              match Ir_jmp.alt jmp with
              | None -> prog
              | Some dst -> match Ir_jmp.resolve dst with
                | Second _ -> prog
                | First dst ->
                  if Option.is_some (Term.find sub_t prog dst)
                  then prog
                  else
                    Term.append sub_t prog @@
                    Ir_sub.create ~tid:dst ())))


let program symtab =
  let b = Ir_program.Builder.create () in
  let sub_of_blk = Hashtbl.create (module Tid) in
  let tid_for_sub =
    let tids = Hash_set.create (module Tid) in
    fun name ->
      let tid = Tid.for_name name in
      match Hash_set.strict_add tids tid with
      | Ok () -> tid
      | Error _ ->
        let tid = Tid.create () in
        Hash_set.strict_add_exn tids tid;
        tid in
  Seq.iter (Symtab.to_sequence symtab) ~f:(fun (name,entry,cfg) ->
      let addr = Block.addr entry in
      let blk_tid = Tid.for_addr addr in
      let sub_tid = tid_for_sub name in
      Tid.set_addr sub_tid addr;
      let sub = lift_sub ~symtab ~tid:sub_tid entry cfg in
      Ir_program.Builder.add_sub b (Ir_sub.with_name sub name);
      Hashtbl.add_exn sub_of_blk ~key:blk_tid ~data:sub_tid;);
  let program = Ir_program.Builder.result b in
  Term.map sub_t program ~f:(fun sub ->
      Term.map blk_t sub ~f:(fun blk ->
          let addr = Term.get_attr blk address in
          Term.map jmp_t blk ~f:(fun jmp ->
              jmp |>
              alternate_nonlocal sub |>
              link_call symtab addr sub_of_blk))) |>
  insert_synthetic

let sub blk cfg = lift_sub blk cfg

let insn ?addr insn =
  let tid = Option.map addr ~f:Tid.for_addr in
  List.rev @@ IrBuilder.lift_insn ?addr insn [Ir_blk.create ?tid ()]

let insns = IrBuilder.insns
