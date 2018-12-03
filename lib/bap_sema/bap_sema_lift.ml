open Core_kernel.Std
open Bap_types.Std
open Graphlib.Std
open Bap_image_std
open Bap_disasm_std
open Bap_ir
open Format
module IRLabel = Label
open Bap_knowledge
module L = Label
module Label = IRLabel

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



let fall_of_block cfg block =
  Seq.find_map (Cfg.Node.outputs block cfg) ~f:(fun e ->
      match Cfg.Edge.label e with
      | `Fall -> Some (Cfg.Edge.dst e)
      | _ -> None)

let label_of_fall cfg block =
  Option.map (fall_of_block cfg block) ~f:(fun blk ->
      Label.indirect Bil.(int (Block.addr blk)))

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
    Semantics.get Insn.Semantics.Domain.bir @@
    Insn.semantics insn

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


  let pp_bir ppf blks =
    Format.pp_print_list Ir_blk.pp ppf blks

  let turn_into_call return b =
    Term.map jmp_t b ~f:(fun jmp ->
        Ir_jmp.with_kind jmp @@
        match Ir_jmp.kind jmp with
        | Goto target -> Call (Call.create ~return ~target ())
        | k -> k)

  let landing_pad return jmp =
    match Ir_jmp.kind jmp with
    | Int (_,pad) ->
      let pad = Ir_blk.create ~tid:pad () in
      let pad = match return with
        | None -> pad
        | Some dst ->
          Term.append jmp_t pad (Ir_jmp.create_goto dst) in
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

  let blk cfg block : blk term list =
    let fall = label_of_fall cfg block in
    let blks =
      Block.insns block |>
      List.fold  ~init:[Ir_blk.create ()] ~f:(fun blks (mem,insn) ->
          lift_insn ~mem insn blks) in
    let blks = with_landing_pads fall blks in
    with_first_blk_addressed (Block.addr block) @@
    match fall with
    | None -> List.rev blks
    | Some dst -> match blks with
      | [] -> []                 (* assert false? *)
      | x :: xs ->
        if Insn.(is call) (Block.terminator block)
        then List.rev (turn_into_call dst x :: xs)
        else
          let fall = Ir_jmp.create_goto dst in
          List.rev (Term.append jmp_t x fall :: xs)
end

let has_jump_under_condition bil =
  with_return (fun {return} ->
      let enter_control ifs = if ifs = 0 then ifs else return true in
      Bil.fold (object
        inherit [int] Stmt.visitor
        method! enter_if ~cond:_ ~yes:_ ~no:_ x = x + 1
        method! leave_if ~cond:_ ~yes:_ ~no:_ x = x - 1
        method! enter_jmp _ ifs    = enter_control ifs
        method! enter_cpuexn _ ifs = enter_control ifs
      end) ~init:0 bil |> fun (_ : int) -> false)

let is_conditional_jump jmp =
  Insn.(may affect_control_flow) jmp &&
  has_jump_under_condition (Insn.bil jmp)


let blk = IrBuilder.blk

let resolve_jmp ~local addrs jmp =
  let update_kind jmp addr make_kind =
    Option.value_map ~default:jmp
      (Hashtbl.find addrs addr)
      ~f:(fun id -> Ir_jmp.with_kind jmp (make_kind id)) in
  match Ir_jmp.kind jmp with
  | Ret _ | Int _ -> jmp
  | Goto (Indirect (Bil.Int addr)) ->
    update_kind jmp addr (fun id ->
        if local then Goto (Direct id)
        else
          Call (Call.create ~target:(Direct id) ()))
  | Goto _ -> jmp
  | Call call ->
    let jmp,call = match Call.target call with
      | _ when local -> jmp, call
      | Indirect (Bil.Int addr) ->
        let new_call = ref call in
        let jmp = update_kind jmp addr
            (fun id ->
               new_call := Call.with_target call (Direct id);
               Call !new_call) in
        jmp, !new_call
      | _ -> jmp,call in
    match Call.return call with
    | Some (Indirect (Bil.Int addr)) when Hashtbl.mem addrs addr ->
      update_kind jmp addr
        (fun id -> Call (Call.with_return call (Direct id)))
    | Some (Indirect (Bil.Int _)) ->
      Ir_jmp.with_kind jmp @@ Call (Call.with_noreturn call)
    | _ -> jmp

(* remove all jumps that are after unconditional jump *)
let remove_false_jmps blk =
  Term.enum jmp_t blk |> Seq.find ~f:(fun jmp ->
      Exp.(Ir_jmp.cond jmp = (Bil.Int Word.b1))) |> function
  | None -> blk
  | Some last ->
    Term.after jmp_t blk (Term.tid last) |> Seq.map ~f:Term.tid |>
    Seq.fold ~init:blk ~f:(Term.remove jmp_t)


let lift_sub entry cfg =
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
  let sub = Ir_sub.Builder.create ?blks:n () in
  List.iter blks ~f:(fun blk ->
      Ir_sub.Builder.add_blk sub
        (Term.map jmp_t blk ~f:(resolve_jmp ~local:true addrs)));
  let sub = Ir_sub.Builder.result sub in
  Term.set_attr sub address (Block.addr entry)

let create_synthetic name =
  let sub = Ir_sub.create ~name () in
  Tid.set_name (Term.tid sub) name;
  Term.(set_attr sub synthetic ())

let indirect_target jmp =
  match Ir_jmp.kind jmp with
  | Ret _ | Int _ | Goto _ -> None
  | Call call -> match Call.target call with
    | Indirect (Bil.Int a) -> Some a
    | _ -> None

let is_indirect_call jmp = Option.is_some (indirect_target jmp)

let with_address t ~f ~default =
  match Term.get_attr t address with
  | None -> default
  | Some a -> f a

let find_call_name symtab blk =
  with_address blk ~default:None ~f:(Symtab.find_call_name symtab)

let update_unresolved symtab unresolved exts sub =
  let iter cls t ~f = Term.to_sequence cls t |> Seq.iter ~f in
  let symbol_exists name =
    Option.is_some (Symtab.find_by_name symtab name) in
  let is_known a = Option.is_some (Symtab.find_by_start symtab a) in
  let is_unknown name = not (symbol_exists name) in
  let add_external name =
    Hashtbl.update exts name ~f:(function
        | None -> create_synthetic name
        | Some x -> x) in
  iter blk_t sub ~f:(fun blk ->
      iter jmp_t blk ~f:(fun jmp ->
          match indirect_target jmp with
          | None -> ()
          | Some a when is_known a -> ()
          | _ ->
            with_address blk ~default:() ~f:(fun addr ->
                Hash_set.add unresolved addr;
                match Symtab.find_call_name symtab addr with
                | Some name when is_unknown name -> add_external name
                | _ -> ())))

let resolve_indirect symtab exts blk jmp =
  let update_target tar =
    match Ir_jmp.kind jmp with
    | Call c -> Ir_jmp.with_kind jmp (Call (Call.with_target c tar))
    | _ -> jmp in
  match find_call_name symtab blk with
  | None -> jmp
  | Some name ->
    match Symtab.find_by_name symtab name with
    | Some (_,b,_) -> update_target (Indirect (Int (Block.addr b)))
    | None ->
      match Hashtbl.find exts name with
      | Some s -> update_target (Direct (Term.tid s))
      | None -> jmp

let program symtab =
  let b = Ir_program.Builder.create () in
  let addrs = Addr.Table.create () in
  let externals = String.Table.create () in
  let unresolved = Addr.Hash_set.create () in
  Seq.iter (Symtab.to_sequence symtab) ~f:(fun (name,entry,cfg) ->
      let addr = Block.addr entry in
      let sub = lift_sub entry cfg in
      Ir_program.Builder.add_sub b (Ir_sub.with_name sub name);
      Tid.set_name (Term.tid sub) name;
      Hashtbl.add_exn addrs ~key:addr ~data:(Term.tid sub);
      update_unresolved symtab unresolved externals sub);
  Hashtbl.iter externals ~f:(Ir_program.Builder.add_sub b);
  let program = Ir_program.Builder.result b in
  let has_unresolved blk =
    with_address blk ~default:false ~f:(Hash_set.mem unresolved) in
  Term.map sub_t program
    ~f:(fun sub -> Term.map blk_t sub ~f:(fun blk ->
        Term.map jmp_t (remove_false_jmps blk)
          ~f:(fun j ->
              let j =
                if is_indirect_call j && has_unresolved blk then
                  resolve_indirect symtab externals blk j
                else j in
              resolve_jmp ~local:false addrs j)))

let sub = lift_sub

let insn insn =
  List.rev @@ IrBuilder.lift_insn insn [Ir_blk.create ()]
