open Bap_core_theory

open Core_kernel[@@warning "-D"]
open Bap_types.Std
open Graphlib.Std
open Bap_image_std
open Bap_disasm_std
open Bap_ir

open KB.Syntax

let update_jmp jmp ~f =
  f (Ir_jmp.dst jmp) (Ir_jmp.alt jmp) @@ fun ~dst ~alt ->
  let jmp' = Ir_jmp.reify
      ~tid:(Term.tid jmp)
      ?cnd:(Ir_jmp.guard jmp)
      ?dst ?alt () in
  Term.with_attrs jmp' (Term.attrs jmp)

let intra_fall cfg block =
  KB.Seq.find_map (Cfg.Node.outputs block cfg) ~f:(fun e ->
      match Cfg.Edge.label e with
      | `Fall ->
        let addr = Word.to_bitvec @@ Block.addr @@ Cfg.Edge.dst e in
        Theory.Label.for_addr addr >>| fun tid ->
        Some (Ir_jmp.resolved tid)
      | _ -> !!None)

let tid_for_name_or_addr symtab name =
  match Symtab.find_by_name symtab name with
  | None -> Theory.Label.for_name name
  | Some (_, entry, _) ->
    Theory.Label.for_addr @@ Word.to_bitvec @@ Block.addr entry

let tid_for_addr = function
  | None -> Theory.Label.fresh
  | Some addr -> Theory.Label.for_addr @@ Word.to_bitvec addr

let maybe_new_tid = function
  | None -> Theory.Label.fresh
  | Some tid -> !!tid

(* a subroutine could be called implicitly via a fallthrough,
   therefore it will not be reified into jmp terms automatically,
   so we need to do some work here.
   We look into the symtable for an implicit call, and if
   such exists and it is physically present in the symtab, then we
   return a tid for the address, otherwise we return a tid for the
   subroutine name. It is important to return the tid for address,
   so that we can compare tids in the [insert_call] function. *)
let inter_fall symtab block = match symtab with
  | None -> !!None
  | Some symtab ->
    match Symtab.implicit_callee symtab (Block.addr block) with
    | None -> !!None
    | Some name ->
      tid_for_name_or_addr symtab name >>|
      Fn.compose Option.some Ir_jmp.resolved

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
    | [],xs | xs,[] -> !!xs
    | x :: xs, y :: ys when def_only x ->
      !!(List.rev_append ys (append_def_only x y :: xs))
    | x::xs, y::_ ->
      Theory.Label.fresh >>| fun tid ->
      let jmp = Ir_jmp.reify ~tid ~dst:(Ir_jmp.resolved @@ Term.tid y) () in
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
    | Int (_,pad) -> begin
        let pad = Ir_blk.create ~tid:pad () in
        match return with
        | Some (`Intra dst) ->
          Theory.Label.fresh >>| fun tid ->
          let jmp = Ir_jmp.reify ~tid ~dst () in
          Some (Term.append jmp_t pad jmp)
        | _ -> !!(Some pad)
      end
    | _ -> !!None

  let with_landing_pads return bs = match bs with
    | [] -> !![]
    | b :: bs as blks ->
      KB.List.fold ~init:[] blks ~f:(fun pads b ->
          Term.enum jmp_t b |>
          KB.Seq.fold ~init:pads ~f:(fun pads jmp ->
              landing_pad return jmp >>| function
              | Some pad -> pad :: pads
              | None -> pads)) >>| fun pads ->
      b :: List.rev_append pads bs

  let resolves_equal x y =
    match Ir_jmp.resolve x, Ir_jmp.resolve y with
    | First x, First y -> Tid.equal x y
    | _ -> false

  let insert_inter_fall alt blk =
    Theory.Label.fresh >>| fun tid ->
    Term.append jmp_t blk @@ Ir_jmp.reify ~tid ~alt ()

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
    then !!blk
    else Theory.Label.fresh >>| fun tid ->
      Term.append jmp_t blk @@
      Ir_jmp.reify ~tid ~dst ()

  let concat_map_fst_and_rev xs f =
    match xs with
    | [] -> !![]
    | x :: xs ->
      f x >>| fun x ->
      List.rev (x @ xs)

  let with_first_blk_optionally_addressed = function
    | None -> fun x -> x
    | Some addr -> with_first_blk_addressed addr

  let maximize ?(is_call=false) ?(is_barrier=false) ?fall = function
    | x when is_barrier -> !![x]
    | x -> match fall with
      | None ->
        !![if is_call || has_call x then turn_into_call None x else x]
      | Some (`Intra dst) ->
        let blk =
          if is_call || has_call x
          then turn_into_call (Some dst) x
          else x in
        fall_if_possible dst blk >>| List.return
      | Some (`Inter dst) ->
        if is_call || has_call x then
          Theory.Label.fresh >>= fun tid ->
          let next = Ir_blk.create ~tid () in
          let fall = Ir_jmp.resolved tid in
          insert_inter_fall dst next >>= fun next ->
          turn_into_call (Some fall) x |>
          fall_if_possible fall >>| fun blk ->
          [next; blk]
        else insert_inter_fall dst x >>| List.return

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
      blk term list -> blk term list KB.t =
    fun ?(is_call=false) ?(is_barrier=false) ?fall ?addr blks ->
    with_landing_pads fall blks >>= fun blks ->
    maximize ~is_call ~is_barrier ?fall |>
    concat_map_fst_and_rev blks >>|
    with_first_blk_optionally_addressed addr

  let insns ?fall ?addr insns =
    tid_for_addr addr >>= fun tid ->
    let init = [Ir_blk.create ~tid ()], None in
    KB.List.fold insns ~init ~f:(fun (blks, _) insn ->
        lift_insn insn blks >>| fun blks ->
        blks, Some insn) >>= function
    | blks, None -> !!blks
    | blks, Some x ->
      pack blks ?fall ?addr
        ~is_call:(Insn.(is call x))
        ~is_barrier:Insn.(is barrier x)

  let fall_of_block symtab cfg block =
    intra_fall cfg block >>= function
    | Some dst -> !!(Some (`Intra dst))
    | None -> inter_fall symtab block >>| function
      | Some dst -> Some (`Inter dst)
      | None -> None

  let blk ?symtab cfg block : blk term list KB.t =
    let addr = Block.addr block in
    Theory.Label.for_addr (Word.to_bitvec addr) >>= fun tid ->
    let init = [Ir_blk.create ~tid ()] in
    Block.insns block |> KB.List.fold ~init ~f:(fun blks (mem,insn) ->
        let addr = Memory.min_addr mem in
        lift_insn ~addr insn blks) >>= fun blks ->
    fall_of_block symtab cfg block >>= fun fall ->
    let x = Block.terminator block in
    pack blks
      ?fall
      ~addr
      ~is_call:(Insn.(is call x) || has_explicit_call symtab block)
      ~is_barrier:Insn.(is barrier x)
end

let blk cfg block = IrBuilder.blk cfg block

let lift_sub ?symtab ?tid entry cfg =
  maybe_new_tid tid >>= fun tid ->
  Graphlib.reverse_postorder_traverse (module Cfg) ~start:entry cfg |>
  Seq.to_list |> KB.List.map ~f:(IrBuilder.blk ?symtab cfg) >>| fun blks ->
  Block.addr entry, List.concat blks, tid

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
  let resolve dst =
    Option.(dst jmp >>| Ir_jmp.resolve >>= function
      | Second _ -> None
      | First tid -> Some tid) in
  let sub_of_dst dst =
    Option.(resolve dst >>= Hashtbl.find sub_of_blk >>| Ir_jmp.resolved) in
  let external_callee () = match addr with
    | None -> !!None
    | Some addr -> match Symtab.explicit_callee symtab addr with
      | None -> !!None
      | Some name ->
        Theory.Label.for_name name >>| fun tid ->
        Some (Ir_jmp.resolved tid) in
  match sub_of_dst Ir_jmp.dst, sub_of_dst Ir_jmp.alt with
  | _, (Some _ as alt) ->
    !!(update_jmp jmp ~f:(fun dst _ jmp -> jmp ~dst ~alt))
  | Some _ as alt, None ->
    !!(update_jmp jmp ~f:(fun _ _ jmp -> jmp ~dst:None ~alt))
  | None,None -> match resolve Ir_jmp.alt with
    | None -> !!jmp
    | Some (_:tid) -> external_callee () >>| function
      | Some alt -> update_jmp jmp ~f:(fun dst _ jmp ->
          jmp ~dst ~alt:(Some alt))
      | None -> jmp

let is_intrinsic sub =
  match KB.Name.package @@ KB.Name.read (Ir_sub.name sub) with
  | "intrinsic" -> true
  | _ -> false

let make_name ?name tid = match name with
  | Some name -> !!name
  | None -> KB.collect Theory.Label.name tid >>= function
    | Some name -> !!name
    | None -> KB.collect Theory.Label.ivec tid >>| function
      | None -> Tid.to_string tid
      | Some ivec -> Format.asprintf "interrupt:#%d" ivec

let create_synthetic ?blks ?name tid =
  make_name ?name tid >>| fun name ->
  let sub = Ir_sub.create ?blks ~name ~tid () in
  let tags = List.filter_opt [
      Some Term.synthetic;
      Option.some_if (is_intrinsic sub) Ir_sub.intrinsic;
    ] in
  List.fold tags ~init:sub ~f:(fun sub tag ->
      Term.set_attr sub tag ())

let has_sub prog tid =
  Option.is_some (Term.find sub_t prog tid)

let insert_synthetic prog =
  Term.enum sub_t prog |>
  KB.Seq.fold ~init:prog ~f:(fun prog sub ->
      Term.enum blk_t sub |>
      KB.Seq.fold ~init:prog ~f:(fun prog blk ->
          Term.enum jmp_t blk |>
          KB.Seq.fold ~init:prog ~f:(fun prog jmp ->
              match Ir_jmp.alt jmp with
              | None -> !!prog
              | Some dst -> match Ir_jmp.resolve dst with
                | Second _ -> !!prog
                | First dst ->
                  if has_sub prog dst then !!prog
                  else create_synthetic dst >>| Term.append sub_t prog)))

let lift_insn ?addr insn =
  tid_for_addr addr >>= fun tid ->
  let init = [Ir_blk.create ~tid ()] in
  IrBuilder.lift_insn ?addr insn init >>| List.rev

let lift_insn_nonempty insn =
  if KB.Value.is_empty insn then !!None
  else lift_insn insn >>| Option.some

let reify_externals symtab prog =
  Symtab.externals symtab |>
  KB.Seq.fold ~init:prog ~f:(fun prog (tid,insn) ->
      if has_sub prog tid then !!prog
      else lift_insn_nonempty insn >>= fun blks ->
        create_synthetic ?blks tid >>| Term.append sub_t prog)

let add_name tid name =
  KB.provide Theory.Label.aliases tid @@
  Set.singleton (module String) name

let set_name_if_possible tid name =
  add_name tid name >>= fun () ->
  KB.collect Theory.Label.name tid >>= function
  | None -> KB.provide Theory.Label.name tid @@ Some name
  | Some _ -> !!()

let mangle_name addr tid name =
  match addr with
  | Some a ->
    sprintf "%s@%s" name @@
    Bap_bitvector.string_of_value ~hex:true a
  | None -> sprintf "%s%%%s" name (Tid.to_string tid)

let mangle_sub s =
  let tid = Term.tid s in
  let attrs = Term.attrs s in
  let addr = Dict.find attrs address in
  let name = mangle_name addr tid (Ir_sub.name s) in
    set_name_if_possible tid name >>| fun () ->
  let s = Ir_sub.create () ~tid ~name
      ~args:(Term.enum arg_t s |> Seq.to_list)
      ~blks:(Term.enum blk_t s |> Seq.to_list) in
  Term.with_attrs s attrs

let fix_names prog =
  let is_new tid name =
    Theory.Label.for_name name >>| Fn.non (Tid.equal tid) in
  let keep_name tids name tid = Map.set tids ~key:name ~data:tid in
  Term.enum sub_t prog |>
  KB.Seq.fold ~init:String.Map.empty ~f:(fun tids sub ->
      let tid = Term.tid sub in
      let name = Ir_sub.name sub in
      match Map.find tids name with
      | None -> !!(keep_name tids name tid)
      | Some _ -> is_new tid name >>| function
        | false -> keep_name tids name tid
        | true -> tids) >>= fun tids ->
  if Term.length sub_t prog = Map.length tids then !!prog
  else Term.KB.map sub_t prog ~f:(fun sub ->
      let tid' = Map.find_exn tids @@ Ir_sub.name sub in
      if Tid.equal tid' @@ Term.tid sub then !!sub
      else mangle_sub sub)

let program symtab =
  Theory.Label.fresh >>= fun prog_tid ->
  let b = Ir_program.Builder.create ~tid:prog_tid () in
  let sub_of_blk = Hashtbl.create (module Tid) in
  let tid_for_sub =
    let tids = Hash_set.create (module Tid) in
    fun name ->
      Theory.Label.for_name name >>= fun tid ->
      match Hash_set.strict_add tids tid with
      | Ok () -> !!tid
      | Error _ ->
        Theory.Label.fresh >>| fun tid ->
        Hash_set.strict_add_exn tids tid;
        tid in
  Symtab.to_sequence symtab |>
  KB.Seq.iter ~f:(fun (name,entry,cfg) ->
      let addr = Word.to_bitvec @@ Block.addr entry in
      Theory.Label.for_addr addr >>= fun blk_tid ->
      tid_for_sub name >>= fun sub_tid ->
      KB.provide Theory.Label.addr sub_tid (Some addr) >>= fun () ->
      lift_sub ~symtab ~tid:sub_tid entry cfg >>= fun (addr, blks, _) ->
      add_name sub_tid name >>| fun () ->
      let sub = Ir_sub.create () ~blks ~tid:sub_tid ~name in
      let sub = Term.set_attr sub address addr in
      Ir_program.Builder.add_sub b sub;
      Hashtbl.add_exn sub_of_blk ~key:blk_tid ~data:sub_tid) >>= fun () ->
  fix_names @@ Ir_program.Builder.result b >>=
  Term.KB.map sub_t ~f:(fun sub ->
      Term.KB.map blk_t sub ~f:(fun blk ->
          let addr = Term.get_attr blk address in
          Term.KB.map jmp_t blk ~f:(fun jmp ->
              let jmp = alternate_nonlocal sub jmp in
              link_call symtab addr sub_of_blk jmp))) >>=
  reify_externals symtab >>= insert_synthetic

let sub blk cfg =
  lift_sub blk cfg >>| fun (addr, blks, tid) ->
  let sub = Ir_sub.create () ~blks ~tid in
  Term.set_attr sub address addr

let insn = lift_insn
let insns = IrBuilder.insns

module KB = struct
  let program = program
  let sub = sub
  let blk = blk
  let insn = insn
  let insns = insns
end

let program symtab =
  let result = Toplevel.var "program" in
  Toplevel.put result @@ KB.program symtab;
  Toplevel.get result

let sub blk cfg =
  let result = Toplevel.var "sub" in
  Toplevel.put result @@ KB.sub blk cfg;
  Toplevel.get result

let blk cfg block =
  let result = Toplevel.var "blk" in
  Toplevel.put result @@ KB.blk cfg block;
  Toplevel.get result

let insn ?addr insn =
  let result = Toplevel.var "insn" in
  Toplevel.put result @@ KB.insn ?addr insn;
  Toplevel.get result

let insns ?fall ?addr insns =
  let result = Toplevel.var "insns" in
  Toplevel.put result @@ KB.insns ?fall ?addr insns;
  Toplevel.get result
