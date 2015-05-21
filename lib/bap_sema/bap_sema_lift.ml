open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std
open Bap_ir

(* A note about lifting call instructions.

   We're labeling calls with an expected continuation, that should be
   derived from the BIL. But instead we lift calls in a rather
   speculative way, thus breaking the abstraction of the BIL, that
   desugars calls into two pseudo instructions:

   <update return address to next instruction>
   <jump to target>

   A correct way of doing things would be to find a live write to the
   place that is used to store return address (ABI specific), and put
   this expression as an expected return address (aka continuation).

   But a short survey into existing instruction sets shows, that call
   instructions doesn't allow to store something other then next
   instruction, e.g., `call` in x86, `bl` in ARM, `jal` in MIPS,
   `call` and `jumpl` in SPARC (althought the latter allows to choose
   arbitrary register to store return address). That's all is not to
   say, that it is impossible to encode a call with return address
   different from a next instruction, that's why it is called a
   speculation.
*)

type linear =
  | Label of tid
  | Instr of Ir_blk.elt

let fall_of_block block =
  Seq.find_map (Block.dests block) ~f:(function
      | `Block (b,`Fall) -> Some b
      | _ -> None)

let label_of_fall block =
  Option.map (fall_of_block block) ~f:(fun blk ->
      Label.indirect Bil.(int (Block.addr blk)))

let linear_of_stmt block insn stmt : linear list =
  let goto ?cond id =
    Ir_blk.Jmp (Ir_jmp.create_goto ?cond (Label.direct id)) in
  let jump ?cond exp =
    let target = Label.indirect exp in
    let fall = lazy (label_of_fall block) in
    let tail = if cond = None then [] else
        match Lazy.force fall with
        | None -> []
        | Some fall -> [Ir_jmp.create_goto fall] in
    if Insn.is_return insn
    then Ir_jmp.create_ret ?cond target :: tail
    else if Insn.is_call insn
    then [
      Ir_jmp.create_call ?cond
        (Call.create ?return:(Lazy.force fall) ~target ())
    ] else Ir_jmp.create_goto ?cond target :: tail in
  let jump ?cond exp =
    List.map (jump ?cond exp) ~f:(fun j -> Instr (Ir_blk.Jmp j)) in
  let cpuexn ?cond n =
    let next = Tid.create () in [
      Instr (Ir_blk.Jmp (Ir_jmp.create_int ?cond n next));
      Label next] in

  let rec linearize = function
    | Bil.Move (lhs,rhs) -> [Instr (Ir_blk.Def (Ir_def.create lhs rhs))]
    | Bil.If (cond, [],[]) -> []
    | Bil.If (cond,[],no) -> linearize Bil.(If (lnot cond, no,[]))
    | Bil.If (cond,[Bil.CpuExn n],[]) -> cpuexn ~cond n
    | Bil.If (cond,[Bil.Jmp exp],[]) -> jump ~cond exp
    | Bil.If (cond,yes,[]) ->
      let yes_label = Tid.create () in
      let tail = Tid.create () in
      Instr (goto ~cond yes_label) ::
      Instr (goto tail) ::
      Label yes_label ::
      List.concat_map yes ~f:linearize @
      Instr (goto tail) ::
      Label tail :: []
    | Bil.If (cond,yes,no) ->
      let yes_label = Tid.create () in
      let no_label = Tid.create () in
      let tail = Tid.create () in
      Instr (goto ~cond yes_label) ::
      Instr (goto no_label) ::
      Label yes_label ::
      List.concat_map yes ~f:linearize @
      Instr (goto tail) ::
      Label no_label ::
      List.concat_map no ~f:linearize @
      Instr (goto tail) ::
      Label tail :: []
    | Bil.Jmp exp -> jump exp
    | Bil.CpuExn n -> cpuexn n
    | Bil.Special _ -> []
    | Bil.While (cond,body) ->
      let header = Tid.create () in
      let tail = Tid.create () in
      let finish = Tid.create () in
      Instr (goto tail) ::
      Label header ::
      List.concat_map body ~f:linearize @
      Instr (goto tail) ::
      Label tail ::
      Instr (goto ~cond header) ::
      Instr (goto finish) ::
      Label finish :: [] in
  linearize stmt

let blk block : blk term list =
  List.fold (Block.insns block) ~init:([],Ir_blk.Builder.create ())
    ~f:(fun init (_mem,insn)->
        List.fold (Insn.bil insn) ~init ~f:(fun init stmt ->
            List.fold (linear_of_stmt block insn stmt) ~init
              ~f:( fun (bs,b) -> function
                  | Label lab ->
                    let b = Ir_blk.Builder.result b in
                    b :: bs,
                    Ir_blk.Builder.create ~tid:lab ()
                  | Instr elt -> Ir_blk.Builder.add_elt b elt; bs,b))) |>
  fun (bs,b) -> match List.rev (Ir_blk.Builder.result b :: bs) with
  | [] -> []
  | b :: bs -> Term.set_attr b Disasm.block (Block.addr block) :: bs

(* extracts resolved calls from the blk *)
let call_of_blk blk =
  Term.to_sequence jmp_t blk |>
  Seq.find_map ~f:(fun jmp -> match Ir_jmp.kind jmp with
      | Int _ | Goto _ | Ret _ -> None
      | Call call -> match Call.target call with
        | Direct _ -> None
        | Indirect (Bil.Int addr) -> Some addr
        | Indirect _ -> None)

let resolve_jmp addrs jmp =
  let update_kind jmp addr make_kind =
    Option.value_map ~default:jmp
      (Hashtbl.find addrs addr)
      ~f:(fun id -> Ir_jmp.with_kind jmp (make_kind id)) in
  match Ir_jmp.kind jmp with
  | Ret _ | Int _ -> jmp
  | Goto (Indirect (Bil.Int addr)) ->
    update_kind jmp addr (fun id -> Goto (Direct id))
  | Goto _ -> jmp
  | Call call ->
    let jmp,call = match Call.target call with
      | Indirect (Bil.Int addr) ->
        let new_call = ref call in
        let jmp = update_kind jmp addr
            (fun id ->
               new_call := Call.with_target call (Direct id);
               Call !new_call) in
        jmp, !new_call
      | _ -> jmp,call in
    match Call.return call with
    | Some (Indirect (Bil.Int addr)) ->
      update_kind jmp addr
        (fun id -> Call (Call.with_return call (Direct id)))
    | _ -> jmp

let sub entry =
  let addrs = Addr.Table.create () in
  let calls = Addr.Hash_set.create () in
  let rec recons acc b =
    let addr = Block.addr b in
    if Hashtbl.mem addrs addr || Hash_set.mem calls addr
    then acc
    else
      let bls = blk b in
      Option.iter (List.hd bls) ~f:(fun blk ->
          Hashtbl.add_exn addrs ~key:addr ~data:(Term.tid blk));
      Option.iter (List.find_map bls ~f:call_of_blk)
        ~f:(Hash_set.add calls);
      Seq.fold (Block.succs b) ~init:(acc @ bls) ~f:recons in
  let blks = recons [] entry in
  let sub = Ir_sub.Builder.create ~blks:(List.length blks) () in
  List.iter blks ~f:(fun blk ->
      Ir_sub.Builder.add_blk sub
        (Term.map jmp_t blk ~f:(resolve_jmp addrs)));
  let sub = Ir_sub.Builder.result sub in
  Term.set_attr sub subroutine_addr (Block.addr entry)

let ok_duplicate = function
  | `Ok | `Duplicate -> ()

let program roots cfg =
  let b = Ir_program.Builder.create () in
  let roots = Bap_sema_symtab.create_roots_table roots cfg in
  let addrs = Addr.Table.create () in
  Hashtbl.iter roots ~f:(fun ~key:root ~data:name ->
      if not (Hashtbl.mem addrs root) then
        match Table.find_addr cfg root with
        | Some (mem,blk)
          when Addr.(Memory.min_addr mem = root) ->
          let sub = sub blk in
          Ir_program.Builder.add_sub b (Ir_sub.with_name sub name);
          Hashtbl.add_exn addrs ~key:root ~data:(Term.tid sub)
        | _ -> ());
  let program = Ir_program.Builder.result b in
  Term.map sub_t program
    ~f:(Term.map blk_t ~f:(Term.map jmp_t ~f:(resolve_jmp addrs)))
