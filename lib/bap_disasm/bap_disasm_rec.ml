open Core_kernel
open Regular.Std
open Bap_types.Std
open Graphlib.Std
open Bap_core_theory
open Bap_image_std

module Dis = Bap_disasm_basic
module Brancher = Bap_disasm_brancher
module Rooter = Bap_disasm_rooter
module Block = Bap_disasm_block
module Insn = Bap_disasm_insn

type full_insn = Dis.full_insn [@@deriving sexp_of]
type insn = Insn.t [@@deriving sexp_of]
type block = Block.t
type edge = Block.edge [@@deriving compare, sexp]
type jump = Block.jump [@@deriving compare, sexp]
type lifter = Bap_disasm_target_factory.lifter

type error = [
  | `Failed_to_disasm of mem
  | `Failed_to_lift of mem * full_insn * Error.t
] [@@deriving sexp_of]

type code = Theory.Program.t [@@deriving bin_io, compare, sexp]
type maybe_insn = full_insn option * code [@@deriving sexp_of]
type decoded = mem * maybe_insn [@@deriving sexp_of]

type dests = Brancher.dests

module Node = struct
  type t = block
  include Opaque.Make(struct
      type nonrec t = t
      let addr = Block.addr
      let compare x y =
        Addr.compare (addr x) (addr y)
      let hash x = Addr.hash (addr x)
    end)
end

module Edge = struct
  type t = edge [@@deriving compare]
  include Opaque.Make(struct
      type nonrec t = t [@@deriving compare]
      let hash = Hashtbl.hash
    end)
end

module Cfg = Graphlib.Make(Node)(Edge)

type cfg = Cfg.t [@@deriving compare]

type blk_dest = [
  | `Block of block * edge
  | `Unresolved of jump
]

(*
   An address is a leader of a basic block if it is
   an instruction, and any of the following is true:
   - it is a root;
   - it is a target of a jump instruction;
   - it has more than one incoming edge;

   An instruction is a terminator of a basic block if
   any of the following is true:
   - it is a jump instruction
   - the next address after the isntruction is:
     - a leader;
     - points to data.
*)
module Stage1 : sig
  type dsts = (addr option * [`Jump | `Cond | `Fall]) list
  type task = private
    | Dest of addr
    | Fall of addr
    | Jump of {src : addr; age: int; dsts : dsts}

  type state = private {
    stop : bool;
    work : task list;           (* work list*)
    addr : addr;                (* current address *)
    begs : Set.M(Addr).t;       (* begins of basic blocks *)
    jmps : dsts Map.M(Addr).t;  (* jumps  *)
    code : Set.M(Addr).t;       (* all valid instructions *)
    data : Set.M(Addr).t        (* all non-instructions *)
  }

  val start : mem -> addr seq -> f:(state -> mem -> state) -> state
  val view : state -> mem -> f:(state -> mem -> state) -> state

  val failed : state -> addr -> state
  val jumped : state -> mem -> dsts -> int -> state
  val stopped : state -> state
  val moved  : state -> mem -> state
  val is_ready : state -> bool
end = struct
  type dsts = (addr option * [`Jump | `Cond | `Fall]) list
  type task =
    | Dest of addr
    | Fall of addr
    | Jump of {src : addr; age: int; dsts : dsts}

  type state = {
    stop : bool;
    work : task list;           (* work list*)
    addr : addr;                (* current address *)
    begs : Set.M(Addr).t;       (* begins of basic blocks *)
    jmps : dsts Map.M(Addr).t;  (* jumps  *)
    code : Set.M(Addr).t;       (* all valid instructions *)
    data : Set.M(Addr).t        (* all non-instructions *)
  }

  let is_code s addr = Set.mem s.code addr
  let is_data s addr = Set.mem s.data addr
  let is_visited s addr = is_code s addr || is_data s addr
  let is_ready s = s.stop

  let rec step s = match s.work with
    | [] ->
      eprintf "This was the last one, stopping\n%!";
      {s with stop = true}
    | Dest next :: work ->
      let s = {s with begs = Set.add s.begs next} in
      eprintf "Marking %a as leader (jump dest)\n"
        Addr.ppo next;
      if is_visited s next then step {s with work}
      else {s with work; addr=next}
    | Fall next :: work ->
      eprintf "processing the fall %a\n" Addr.ppo next;
      if is_code s next
      then step {s with begs = Set.add s.begs next; work}
      else if is_data s next then step {s with work}
      else {s with work; addr=next}
    | Jump {src; dsts} :: ([] as work)
    | Jump {src; dsts; age=0} :: work ->
      eprintf "Jmp from %a is retired\n" Addr.ppo src;
      let init = {s with jmps = Map.add_exn s.jmps src dsts; work} in
      step @@
      List.fold dsts ~init ~f:(fun s -> function
          | Some next,(`Jump|`Cond) when not (is_visited s next) ->
            eprintf "retire: pushing %a\n" Addr.ppo next;
            {s with work = Dest next :: s.work}
          | _ -> s)
    | Jump jmp :: Fall next :: work ->
      assert (jmp.age > 0);
      eprintf "delaying jump...\n";
      step {
        s with
        work = Fall next :: Jump {
            jmp with age = jmp.age-1; src = next;
          } :: work
      }
    | Jump jmp :: work ->
      eprintf "Warning: control instruction in the delay slot\n%!";
      step {
        s with work = Jump {jmp with age=0} :: work
      }

  let decoded s mem =
    eprintf "decoded %a\n" Memory.ppo mem;
    {
      s with code = Set.add s.code (Memory.min_addr mem)
    }

  let jumped s mem dsts delay =
    let s = decoded s mem in
    eprintf "jumped: pushing %a, Jump\n"
      Addr.ppo ((Addr.succ (Memory.max_addr mem)));
    let next = Fall (Addr.succ (Memory.max_addr mem)) in
    let jump = Jump {src = Memory.min_addr mem; age=delay; dsts} in
    step {s with work = jump :: next :: s.work }

  let moved s mem =
    let s = decoded s mem in
    eprintf "moved: pushing %a\n" Addr.ppo (Addr.succ (Memory.max_addr mem));
    step {
      s with work = Fall (Addr.succ (Memory.max_addr mem)) :: s.work;
    }

  let failed s addr =
    let work = if is_visited s (Addr.succ addr) then s.work
      else Dest (Addr.succ addr) :: s.work in
    step {
      s with data = Set.add s.data addr; work
    }

  let stopped s = step s

  let rec view s base ~f = match Memory.view ~from:s.addr base with
    | Ok mem -> f s mem
    | Error _ ->
      let s = {
        s with
        begs = Set.remove s.begs s.addr;
        jmps = Map.remove s.jmps s.addr;
      } in
      match s.work with
      | [] ->
        eprintf "view: no more tasks, stopping\n";
        step s
      | _  -> view (step s) base ~f

  let start mem roots =
    let roots = Seq.fold roots ~init:(Set.empty (module Addr)) ~f:Set.add in
    let work =
      Set.to_sequence ~order:`Decreasing roots |>
      Seq.fold ~init:[] ~f:(fun work root -> Dest root :: work) in
    let start = match Set.min_elt roots with
      | Some start -> start
      | None -> Memory.min_addr mem in
    eprintf "staring, %d tasks in list\n" (List.length work);
    view {
      work;
      addr = start;
      stop = false;
      begs = Set.empty (module Addr);
      jmps = Map.empty (module Addr);
      code = Set.empty (module Addr);
      data = Set.empty (module Addr);
    } mem
end

type t = {
  cfg  : Cfg.t;
  base : mem;
  data : Set.M(Addr).t;
}

let rec has_jump = function
  | [] -> false
  | Bil.Jmp _ :: _ | Bil.CpuExn _ :: _ -> true
  | Bil.If (_,y,n) :: xs -> has_jump y || has_jump n || has_jump xs
  | Bil.While (_,b) :: xs -> has_jump b || has_jump xs
  | _ :: xs -> has_jump xs


let delay insn = match KB.Value.get Insn.Slot.delay insn with
  | None -> 0
  | Some x -> x

let stop_on = [`Valid]

let stage1 ?(rooter=Rooter.empty) lift brancher disasm base =
  let steps = ref 0 in
  let step d s =
    incr steps;
    let {Stage1.work} = s in
    eprintf "step %d: %d steps left\n" steps.contents
      (List.length work);
    if Stage1.is_ready s then s
    else Stage1.view s base ~f:(fun s mem -> Dis.jump d mem s) in
  Stage1.start base (Rooter.roots rooter) ~f:(fun init mem ->
      Dis.run disasm mem ~stop_on ~return:ident ~init
        ~stopped:(fun d s -> step d (Stage1.stopped s))
        ~hit:(fun d mem insn s ->
            step d @@
            let code = lift mem insn in
            if Insn.(may affect_control_flow) code ||
               has_jump (Insn.bil code)
            then
              Stage1.jumped s mem (brancher mem insn) (delay code)
            else Stage1.moved s mem)
        ~invalid:(fun d _ s ->  step d (Stage1.failed s s.addr)))

let reorder_insns delay insns =
  let rec loop delayed result input =
    let delayed,result = List.fold delayed
        ~init:([],result) ~f:(fun (delayed,result) (delay,insn) ->
            if delay = 0 then delayed, insn::result
            else (delay-1,insn)::delayed, result) in
    match input with
    | x :: xs ->
      let delay = delay x in
      if delay = 0 then loop delayed (x::result)  xs
      else loop ((delay,x)::delayed) result xs
    | [] -> match delayed with
      | [] -> List.rev result
      | _ -> loop delayed result [] in
  loop [] [] insns

let build_graph dis lift base {Stage1.begs; jmps; data} =
  eprintf "Stage1 finished with %d blocks\n" (Set.length begs);
  let blocks = Hashtbl.create (module Addr) in
  let view ?len from = match Memory.view ?words:len ~from base with
    | Ok mem -> mem
    | Error _ ->
      eprintf "Failed to create a view [%a:%s] from [%a:%d]\n"
        Addr.ppo from (match len with None -> "" |
            Some s -> string_of_int s)
        Addr.ppo (Memory.min_addr base)
        (Memory.length base);
      assert false in
  let rec build cfg beg = match Hashtbl.find blocks beg with
    | Some block -> cfg,block
    | None ->
      let fin,len,insns =
        Dis.run dis (view beg) ~stop_on ~init:(beg,0,[]) ~return:ident
          ~hit:(fun dis mem insn (curr,len,insns) ->
              let len = Memory.length mem + len in
              let insn = mem,lift mem insn in
              let last = Memory.max_addr mem in
              let next = Addr.succ last in
              if Set.mem begs next ||
                 Map.mem jmps curr ||
                 Set.mem data next ||
                 Addr.equal last (Memory.max_addr base)
              then (curr,len,insn::insns)
              else Dis.jump dis (view next) (next,len,insn::insns)) in
      let insns = reorder_insns (fun (_,x) -> delay x) (List.rev insns) in
      let block = Block.create (view ~len beg) insns in
      Hashtbl.add_exn blocks beg block;
      match Map.find jmps fin with
      | None -> Cfg.Node.insert block cfg,block
      | Some dsts ->
        List.fold dsts ~init:cfg ~f:(fun cfg -> function
            | None,_ -> cfg
            | Some dst,kind ->
              let dst = match kind with
                | `Fall ->
                  Addr.succ (Memory.max_addr (Block.memory block))
                | _ -> dst in
              let cfg,dst = build cfg dst in
              let edge = Cfg.Edge.create block dst kind in
              Cfg.Edge.insert edge cfg),block  in
  let cfg = Set.fold begs ~init:Cfg.empty ~f:(fun cfg beg ->
      fst (build cfg beg)) in
  eprintf "CFG reconstructed, got %d blocks out of %d expected\n"
    (Cfg.number_of_nodes cfg)
    (Set.length begs);
  cfg


let finish dis lifter base stage1 = Ok {
    cfg = build_graph dis lifter base stage1;
    base;
    data = Set.empty (module Addr);
  }

let lifter arch mem insn =
  let open KB.Syntax in
  let code =
    KB.Object.create Theory.Program.cls >>= fun code ->
    KB.provide Arch.slot code (Some arch) >>= fun () ->
    KB.provide Memory.slot code (Some mem) >>= fun () ->
    KB.provide Insn.Slot.name code (Dis.Insn.name insn) >>= fun () ->
    KB.provide Dis.Insn.slot code (Some insn) >>| fun () ->
    code in
  match Bap_state.run Theory.Program.cls code with
  | Ok prog ->
    let bil = Insn.bil prog in
    let prog' = Insn.of_basic ~bil insn in
    KB.Value.merge ~on_conflict:`drop_right prog prog'
  | Error _ ->
    KB.Value.empty Theory.Program.cls

let run ?(backend="llvm") ?brancher ?rooter arch mem =
  let b = Option.value brancher ~default:(Brancher.of_bil arch) in
  let brancher = Brancher.resolve b in
  let lifter = lifter arch in
  Dis.with_disasm ~backend (Arch.to_string arch) ~f:(fun dis ->
      finish dis lifter mem @@
      stage1 ?rooter lifter brancher dis mem)

let cfg t = t.cfg
let errors s =
  Set.fold s.data ~init:[] ~f:(fun errs addr ->
      match Memory.view s.base ~from:addr ~words:1 with
      | Error _ -> errs
      | Ok mem -> `Failed_to_disasm mem :: errs)



(* let with_bil : code -> bil -> code = fun code bil ->
 *   let slot = Theory.Program.Semantics.slot in
 *   KB.Value.put Theory.Program.Semantics.slot code @@
 *   KB.Value.put Bil.slot (KB.Value.get slot code) bil *)

(* let sexp_of_addr addr =
 *   Sexp.Atom (Addr.string_of_value addr) *)
(* a set of all known jump destinations *)
(* let collect_dests (dests : dests Addr.Table.t) =
 *   let addrs = Hash_set.create (module Addr) () in
 *   Addrs.iter dests ~f:(List.iter ~f:(function
 *       | Some dst,_  when not (Hashtbl.mem dests dst) ->
 *         Hash_set.add addrs dst
 *       | _ -> ()));
 *   addrs
 *
 * let join_destinations ?default dests =
 *   let jmp x = [ Bil.(jmp (int x)) ] in
 *   let undecided x =
 *     Bil.if_ (Bil.unknown "destination" (Type.Imm 1)) (jmp x) [] in
 *   let init = match default with
 *     | None -> []
 *     | Some x -> jmp x in
 *   Set.fold dests ~init ~f:(fun bil x -> undecided x :: bil)
 *
 * let make_switch x dests =
 *   let case addr = Bil.(if_ (x = int addr) [jmp (int addr)] []) in
 *   let default = Bil.jmp x in
 *   Set.fold dests ~init:[default] ~f:(fun ds a -> case a :: ds)
 *
 * let dests_of_bil bil =
 *   (object inherit [Addr.Set.t] Stmt.visitor
 *     method! visit_jmp e dests = match e with
 *       | Int w -> Set.add dests w
 *       | _ -> dests
 *   end)#run bil Addr.Set.empty
 *
 * let add_destinations bil = function
 *   | [] -> bil
 *   | dests ->
 *     let d = dests_of_bil bil in
 *     let d' = Addr.Set.of_list dests in
 *     let n = Set.diff d' d in
 *     if Set.is_empty n then bil
 *     else
 *     if has_jump bil then
 *       (object inherit Stmt.mapper
 *         method! map_jmp = function
 *           | Int addr -> join_destinations ~default:addr n
 *           | indirect -> make_switch indirect n
 *       end)#run bil
 *     else bil @ join_destinations n *)

(* let disasm stage1 dis mem =
 *   Dis.run dis mem
 *     ~init:[] ~return:ident ~stopped:(fun s _ ->
 *         Dis.stop s (Dis.insns s)) |>
 *   List.map ~f:(function
 *       | mem, None -> mem,(None,Theory.Program.empty)
 *       | mem, (Some ins as insn) ->
 *         (\* let dests =
 *          *   Addrs.find stage1.dests (Memory.min_addr mem) |>
 *          *   Option.value ~default:[] |>
 *          *   List.filter_map ~f:(function
 *          *       | a, (`Cond | `Jump) -> a
 *          *       | _ -> None) in
 *          * let bil,sema = match stage1.lift mem ins with
 *          *   | Ok sema -> Insn.bil sema, sema
 *          *   | _ -> [], Theory.Program.empty in
 *          * let bil = add_destinations bil dests in *\)
 *         mem, (insn, with_bil sema bil)) *)
